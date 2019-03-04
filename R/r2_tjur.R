#' Tjur's (2009) R2 - coefficient of determination (D)
#'
#' This method calculates the Coefficient of Discrimination \code{D} (also known as Tjur's R2; Tjur, 2009) for generalized linear (mixed) models for binary outcomes. It is an alternative to other Pseudo-R-squared values like Nagelkerke's R2 or Cox-Snell R2. The Coefficient of Discrimination \code{D} can be read like any other (Pseudo-)R-squared value.
#'
#' @param model Binomial Model.
#'
#' @examples
#' model <- glm(vs ~ wt + mpg, data = mtcars, family = "binomial")
#' r2_tjur(model)
#' \dontrun{
#' library(rstanarm)
#' model <- rstanarm::stan_glm(vs ~ wt + mpg, data = mtcars, family = "binomial")
#' }
#'
#' @importFrom stats predict residuals
#' @importFrom insight get_response model_info
#'
#' @references Tjur, T. (2009). Coefficients of determination in logistic regression models—A new proposal: The coefficient of discrimination. The American Statistician, 63(4), 366-372.
#'
#' @export
r2_tjur <- function(model) {
  # check for valid object class
  if (!insight::model_info(model)$is_binomial) {
    stop("`model` must be binomial.")
  }

  y <- insight::get_response(model)
  pred <- stats::predict(model, type = "response", re.form = NULL)

  # TODO: replace for get_residuals?
  # delete pred for cases with missing residuals
  if (anyNA(stats::residuals(model))) {
    pred <- pred[!is.na(stats::residuals(model))]
  }

  categories <- unique(y)
  m1 <- mean(pred[which(y == categories[1])], na.rm = TRUE)
  m2 <- mean(pred[which(y == categories[2])], na.rm = TRUE)

  abs(m2 - m1)
}
