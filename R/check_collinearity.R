#' @title Check for multicollinearity of model predictors
#' @name check_collinearity
#'
#' @description \code{check_collinearity()} checks regression models for
#'   multicollinearity by calculating the variance inflation factor (VIF).
#'
#' @param x A model object (that should at least respond to \code{vcov()},
#'  and if possible, also to \code{model.matrix()} - however, it also should
#'  work without \code{model.matrix()}).
#' @param component For models with zero-inflation component, multicollinearity
#'  can be checked for the conditional model (count component,
#'  \code{component = "conditional"} or \code{component = "count"}) or
#'  zero-inflation component (\code{component = "zero_inflated"} or
#'  \code{component = "zi"}).
#' @param ... Currently not used.
#'
#' @return A data frame with three columns: The name of the model term, the
#'   variance inflation factor and the factor by which the standard error
#'   is increased due to possible correlation with other predictors.
#'
#' @details The variance inflation factor is a measure to analyze the magnitude
#'   of multicollinearity of model predictors. A VIF less than 5 indicates
#'   a low correlation of that predictor with other predictors. A value between
#'   5 and 10 indicates a moderate correlation, while VIF values larger than 10
#'   are a sign for high, not tolerable correlation of model predictors. The
#'   \emph{Increased SE} column in the output indicates how much larger
#'   the standard error is due to the correlation with other predictors.
#'
#' @references James, G., Witten, D., Hastie, T., & Tibshirani, R. (Hrsg.). (2013). An introduction to statistical learning: with applications in R. New York: Springer.
#'
#' @examples
#' m <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
#' check_collinearity(m)
#'
#' @importFrom stats vcov cov2cor
#' @importFrom insight has_intercept find_predictors
#' @export
check_collinearity <- function(x, ...) {
  UseMethod("check_collinearity")
}


#' @export
check_collinearity.default <- function(x, ...) {
  .check_collinearity(x, component = "conditional")
}



#' @export
check_collinearity.glmmTMB <- function(x, component = c("conditional", "count", "zi", "zero_inflated"), ...) {
  component <- match.arg(component)

  if (component == "count") component <- "conditional"
  if (component == "zi") component <- "zero_inflated"

  .check_collinearity(x, component)
}



#' @export
check_collinearity.hurdle <- function(x, component = c("conditional", "count", "zi", "zero_inflated"), ...) {
  component <- match.arg(component)

  if (component == "count") component <- "conditional"
  if (component == "zi") component <- "zero_inflated"

  .check_collinearity(x, component)
}



#' @export
check_collinearity.zeroinfl <- function(x, component = c("conditional", "count", "zi", "zero_inflated"), ...) {
  component <- match.arg(component)

  if (component == "count") component <- "conditional"
  if (component == "zi") component <- "zero_inflated"

  .check_collinearity(x, component)
}



#' @export
check_collinearity.zerocount <- function(x, component = c("conditional", "count", "zi", "zero_inflated"), ...) {
  component <- match.arg(component)

  if (component == "count") component <- "conditional"
  if (component == "zi") component <- "zero_inflated"

  .check_collinearity(x, component)
}



#' @keywords internal
.check_collinearity <- function(x, component) {
  v <- .vcov_as_matrix(x, component)
  assign <- .term_assignments(x, component)

  if (insight::has_intercept(x)) {
    v <- v[-1, -1]
    assign <- assign[-1]
  }

  terms <- insight::find_predictors(x)[[component]]
  n.terms <- length(terms)

  if (n.terms < 2) {
    warning("Not enought model terms to check for multicollinearity.")
    return(NULL)
  }

  R <- stats::cov2cor(v)
  detR <- det(R)

  result <- vector("numeric")

  for (term in 1:n.terms) {
    subs <- which(assign == term)
    result <- c(
      result,
      det(as.matrix(R[subs, subs])) * det(as.matrix(R[-subs, -subs])) / detR
    )
  }

  structure(
    class = c("check_collinearity", "data.frame"),
    data.frame(
      Predictor = terms,
      VIF = result,
      SE_factor = sqrt(result),
      stringsAsFactors = FALSE
    )
  )
}



#' @importFrom stats vcov
#' @keywords internal
.vcov_as_matrix <- function(x, component) {
  if (inherits(x, c("hurdle", "zeroinfl", "zerocount"))) {
    switch(
      component,
      conditional = as.matrix(stats::vcov(x, model = "count")),
      zero_inflated = as.matrix(stats::vcov(x, model = "zero"))
    )
  } else {
    switch(
      component,
      conditional = as.matrix(.collapse_cond(stats::vcov(x))),
      zero_inflated = as.matrix(.collapse_zi(stats::vcov(x)))
    )
  }
}



#' @importFrom stats model.matrix
#' @keywords internal
.term_assignments <- function(x, component) {
  tryCatch({
    if (inherits(x, c("hurdle", "zeroinfl", "zerocount"))) {
      assign <- switch(
        component,
        conditional = attr(stats::model.matrix(x, model = "count"), "assign"),
        zero_inflated = attr(stats::model.matrix(x, model = "zero"), "assign")
      )
    } else {
      assign <- attr(stats::model.matrix(x), "assign")
    }

    if (is.null(assign)) {
      assign <- .find_term_assignment(x, component)
    }

    assign
  },
  error = function(e) {
    .find_term_assignment(x, component)
  })
}



#' @importFrom insight find_parameters find_predictors
#' @keywords internal
.find_term_assignment <- function(x, component) {
  match(
    insight::clean_names(insight::find_parameters(x)[[component]]),
    insight::find_predictors(x)[[component]]
  )
}


.collapse_zi <- function(x) {
  if (is.list(x) && "zi" %in% names(x)) {
    x[["zi"]]
  } else {
    x
  }
}