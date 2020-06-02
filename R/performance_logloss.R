#' @title Log Loss
#' @name performance_logloss
#'
#' @description Compute the log loss for models with binary outcome.
#'
#' @param model Model with binary outcome.
#' @param ... Currently not used.
#' @inheritParams model_performance.lm
#'
#' @return Numeric, the log loss of \code{model}.
#'
#' @details Logistic regression models predict the probability of an outcome of
#'   being a "success" or "failure" (or 1 and 0 etc.). \code{performance_logloss()} evaluates
#'   how good or bad the predicted probabilities are. High values indicate
#'   bad predictions, while low values indicate good predictions. The lower
#'   the log-loss, the better the model predicts the outcome.
#'
#' @seealso \code{\link[=performance_score]{performance_score()}}
#'
#' @examples
#' data(mtcars)
#' m <- glm(formula = vs ~ hp + wt, family = binomial, data = mtcars)
#' performance_logloss(m)
#' @importFrom stats fitted
#' @importFrom insight get_response print_color
#' @export
performance_logloss <- function(model, verbose = TRUE, ...) {
  UseMethod("performance_logloss")
}


#' @export
performance_logloss.default <- function(model, verbose = TRUE, ...) {
  resp <- .factor_to_numeric(insight::get_response(model))
  ll <- suppressWarnings(mean(log(1 - abs(resp - stats::fitted(model))) * -1))

  if (is.na(ll)) {
    if (verbose) insight::print_color("Can't calculate log-loss.\n", "red")
    return(NA)
  }

  ll
}


#' @export
performance_logloss.brmsfit <- function(model, verbose = TRUE, ...) {
  yhat <- stats::fitted(object = model, summary = TRUE, ...)[, "Estimate"]
  resp <- .factor_to_numeric(insight::get_response(model))
  ll <- suppressWarnings(mean(log(1 - abs(resp - yhat)) * -1))

  if (is.na(ll)) {
    if (verbose) insight::print_color("Can't calculate log-loss.\n", "red")
    return(NA)
  }

  ll
}



# mfx models -------------------------------

#' @export
performance_logloss.logitor <- function(model, ...) {
  performance_logloss(model$fit, ...)
}

#' @export
performance_logloss.logitmfx <- performance_logloss.logitor

#' @export
performance_logloss.probitmfx <- performance_logloss.logitor
