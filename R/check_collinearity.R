#' @title Check for multicollinearity of model terms
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
#'  \code{component = "conditional"} or \code{component = "count"}),
#'  zero-inflation component (\code{component = "zero_inflated"} or
#'  \code{component = "zi"}) or both components (\code{component = "all"}).
#'  Following model-classes are currently supported: \code{hurdle},
#'  \code{zeroinfl}, \code{zerocount}, \code{MixMod} and \code{glmmTMB}.
#' @param ... Currently not used.
#'
#' @return A data frame with three columns: The name of the model term, the
#'   variance inflation factor and the factor by which the standard error
#'   is increased due to possible correlation with other terms.
#'
#' @details The variance inflation factor is a measure to analyze the magnitude
#'   of multicollinearity of model terms. A VIF less than 5 indicates
#'   a low correlation of that predictor with other predictors. A value between
#'   5 and 10 indicates a moderate correlation, while VIF values larger than 10
#'   are a sign for high, not tolerable correlation of model predictors. The
#'   \emph{Increased SE} column in the output indicates how much larger
#'   the standard error is due to the correlation with other predictors.
#'   \cr \cr
#'   An informative blog post about collinearity can be found
#'   \href{https://janhove.github.io/analysis/2019/09/11/collinearity}{here}.
#'
#' @references James, G., Witten, D., Hastie, T., & Tibshirani, R. (Hrsg.). (2013). An introduction to statistical learning: with applications in R. New York: Springer.
#'
#' @examples
#' m <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
#' check_collinearity(m)
#'
#' # plot results
#' x <- check_collinearity(m)
#' plot(x)
#' @importFrom stats vcov cov2cor terms
#' @importFrom insight has_intercept find_formula model_info print_color
#' @export
check_collinearity <- function(x, ...) {
  UseMethod("check_collinearity")
}


#' @export
check_collinearity.default <- function(x, ...) {
  .check_collinearity(x, component = "conditional")
}



#' @rdname check_collinearity
#' @export
check_collinearity.glmmTMB <- function(x, component = c("all", "conditional", "count", "zi", "zero_inflated"), ...) {
  component <- match.arg(component)
  .check_collinearity_zi_model(x, component)
}



#' @export
check_collinearity.MixMod <- function(x, component = c("all", "conditional", "count", "zi", "zero_inflated"), ...) {
  component <- match.arg(component)
  .check_collinearity_zi_model(x, component)
}



#' @export
check_collinearity.hurdle <- function(x, component = c("all", "conditional", "count", "zi", "zero_inflated"), ...) {
  component <- match.arg(component)
  .check_collinearity_zi_model(x, component)
}



#' @export
check_collinearity.zeroinfl <- function(x, component = c("all", "conditional", "count", "zi", "zero_inflated"), ...) {
  component <- match.arg(component)
  .check_collinearity_zi_model(x, component)
}



#' @export
check_collinearity.zerocount <- function(x, component = c("all", "conditional", "count", "zi", "zero_inflated"), ...) {
  component <- match.arg(component)
  .check_collinearity_zi_model(x, component)
}



.check_collinearity_zi_model <- function(x, component) {
  if (component == "count") component <- "conditional"
  if (component == "zi") component <- "zero_inflated"

  mi <- insight::model_info(x)
  if (!mi$is_zero_inflated) component <- "conditional"

  if (component == "all") {
    cond <- .check_collinearity(x, "conditional")
    zi <- .check_collinearity(x, "zero_inflated")
    if (is.null(cond) && is.null(zi)) {
      return(NULL)
    }
    if (is.null(cond)) {
      zi$Component <- "zero inflated"
      return(zi)
    }
    if (is.null(zi)) {
      cond$Component <- "conditional"
      return(cond)
    }
    cond$Component <- "conditional"
    zi$Component <- "zero inflated"
    dat_cond <- attr(cond, "data")
    dat_zi <- attr(zi, "data")
    dat <- rbind(cond, zi)
    attr(dat, "data") <- rbind(dat_cond, dat_zi)
    dat
  } else {
    .check_collinearity(x, component)
  }
}




#' @importFrom insight get_varcov
.check_collinearity <- function(x, component) {
  v <- insight::get_varcov(x, component = component)
  assign <- .term_assignments(x, component)

  if (insight::has_intercept(x)) {
    v <- v[-1, -1]
    assign <- assign[-1]
  } else {
    warning("Model has no intercept. VIFs may not be sensible.", call. = FALSE)
  }

  f <- insight::find_formula(x)

  if (inherits(x, "mixor")) {
    terms <- labels(x$terms)
  } else {
    terms <- labels(stats::terms(f[[component]]))
  }

  if ("instruments" %in% names(f)) {
    terms <- unique(c(terms, labels(stats::terms(f[["instruments"]]))))
  }

  n.terms <- length(terms)

  if (n.terms < 2) {
    insight::print_color(sprintf("Not enough model terms in the %s part of the model to check for multicollinearity.\n", component), "red")
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
    class = c("check_collinearity", "see_check_collinearity", "data.frame"),
    .remove_backticks_from_parameter_names(
      data.frame(
        Parameter = terms,
        VIF = result,
        SE_factor = sqrt(result),
        stringsAsFactors = FALSE
      )
    ),
    data = .remove_backticks_from_parameter_names(
      data.frame(
        Parameter = terms,
        VIF = result,
        SE_factor = sqrt(result),
        Component = component,
        stringsAsFactors = FALSE
      )
    )
  )
}




#' @importFrom stats model.matrix
.term_assignments <- function(x, component) {
  tryCatch(
    {
      if (inherits(x, c("hurdle", "zeroinfl", "zerocount"))) {
        assign <- switch(
          component,
          conditional = attr(stats::model.matrix(x, model = "count"), "assign"),
          zero_inflated = attr(stats::model.matrix(x, model = "zero"), "assign")
        )
      } else if (inherits(x, "glmmTMB")) {
        assign <- switch(
          component,
          conditional = attr(stats::model.matrix(x), "assign"),
          zero_inflated = .find_term_assignment(x, component)
        )
      } else if (inherits(x, "MixMod")) {
        assign <- switch(
          component,
          conditional = attr(stats::model.matrix(x, type = "fixed"), "assign"),
          zero_inflated = attr(stats::model.matrix(x, type = "zi_fixed"), "assign")
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
    }
  )
}




#' @importFrom insight get_data find_predictors find_parameters clean_names
.find_term_assignment <- function(x, component) {
  pred <- insight::find_predictors(x)[[component]]
  dat <- insight::get_data(x)[, pred]

  parms <- unlist(lapply(1:length(pred), function(i) {
    p <- pred[i]
    if (is.factor(dat[[p]])) {
      ps <- paste0(p, levels(dat[[p]]))
      names(ps)[1:length(ps)] <- i
      ps
    } else {
      names(p) <- i
      p
    }
  }))

  as.numeric(names(parms)[match(
    insight::clean_names(insight::find_parameters(x)[[component]]),
    parms
  )])
}
