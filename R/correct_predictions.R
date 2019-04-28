#' @title Expected Percentage of Correct Predictions
#' @name correct_predictions
#'
#' @description Expected percentage of correct predictions (ePCP) for models
#'   with binary outcome.
#'
#' @param model Model with binary outcome.
#' @param ci The level of the confidence interval.
#'
#' @return A data frame with three columns: the expected percentage of
#'   correct predictions and the related lower and upper confidence interval.
#'
#' @details The ePCP ranges from 0 to 1, where values closer to 1 mean that
#'   the model predicts the outcome better than models with an eCPC closer to 0.
#'
#' @examples
#' data(mtcars)
#' m <- glm(formula = vs ~ hp + wt, family = binomial, data = mtcars)
#' correct_predictions(m)
#'
#' @references Herron, M. (1999). Postestimation Uncertainty in Limited Dependent Variable Models. Political Analysis, 8, 83–98.
#'
#' @importFrom stats predict qnorm
#' @importFrom insight get_response n_obs model_info
#' @export
correct_predictions <- function(model, ci = 0.95) {

  mi <- insight::model_info(model)

  if (!mi$is_binomial) {
    stop("`correct_predictions()` only works for models with binary outcome.")
  }

  y <- insight::get_response(model)
  n <- insight::n_obs(model)
  pr <- stats::predict(model, type = "response")

  epcp <- (sum(1 - pr[y == 0]) + sum(pr[y == 1])) / n

  data.frame(
    epcp,
    CI_low = epcp - stats::qnorm((1 + ci) / 2) * sqrt(epcp * (1 - epcp) / n),
    CI_high = epcp + stats::qnorm((1 + ci) / 2) * sqrt(epcp * (1 - epcp) / n)
  )
}
