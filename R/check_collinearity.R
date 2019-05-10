#' @title Check for multicollinearity of model predictors
#' @name check_collinearity
#'
#' @description \code{check_collinearity()} checks regression models for
#'   multicollinearity by calculating the variance inflation factor (VIF).
#'
#' @param x A model object (that should respond to \code{vcov()} and \code{model.matrix()}.
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
#' @export
check_collinearity <- function(x, ...) {
  UseMethod("check_collinearity")
}


#' @importFrom stats vcov cov2cor
#' @importFrom insight has_intercept find_predictors
#' @export
check_collinearity.default <- function(x, ...) {
  v <- as.matrix(stats::vcov(x))
  assign <- attr(stats::model.matrix(x), "assign")

  if (insight::has_intercept(x)) {
    v <- v[-1, -1]
    assign <- assign[-1]
  }

  terms <- insight::find_predictors(x)[["conditional"]]
  n.terms <- length(terms)

  if (n.terms < 2) {
    warning("Not enought model terms to check for multicollinearity.")
    return(NA)
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


#' @importFrom stats vcov cov2cor
#' @importFrom insight has_intercept find_predictors
#' @export
check_collinearity.glmmTMB <- function(x, ...) {
  v <- as.matrix(.collapse_cond(stats::vcov(x)))
  assign <- attr(stats::model.matrix(x), "assign")

  if (insight::has_intercept(x)) {
    v <- v[-1, -1]
    assign <- assign[-1]
  }

  terms <- insight::find_predictors(x)[["conditional"]]
  n.terms <- length(terms)

  if (n.terms < 2) {
    warning("Not enought model terms to check for multicollinearity.")
    return(NA)
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


## assign <- match(insight::clean_names(insight::find_parameters(m)[["conditional"]]), insight::find_predictors(m)[["conditional"]])
