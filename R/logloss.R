#' @title Log Loss
#' @name log_loss
#'
#' @description Compute the log loss for models with binary outcome.
#'
#' @param model Model with binary outcome.
#' @param ... Currently not used.
#'
#' @return Numeric, the log loss of \code{model}.
#'
#' @details Logistic regression models predict the probability of an outcome of
#'   being a "success" or "failure" (or 1 and 0 etc.). \code{log_loss()} evaluates
#'   how good or bad the predicted probabilities are. High values indicate
#'   bad predictions, while low values indicate good predictions. The lower
#'   the log-loss, the better the model predicts the outcome.
#'
#' @examples
#' data(mtcars)
#' m <- glm(formula = vs ~ hp + wt, family = binomial, data = mtcars)
#' log_loss(m)
#'
#' @importFrom stats fitted
#' @importFrom insight get_response
#' @export
log_loss <- function(model, ...) {
  UseMethod("log_loss")
}


#' @export
log_loss.default <- function(model, ...) {
  mean(log(1 - abs(insight::get_response(model) - stats::fitted(model))) * -1)
}


#' @export
log_loss.brmsfit <- function(model, ...) {
  yhat <- stats::fitted(object = model, summary = TRUE, ...)[, "Estimate"]
  mean(log(1 - abs(insight::get_response(model) - yhat)) * -1)
}
