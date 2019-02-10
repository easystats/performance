#' Tjur's (2009) coefficient of determination (D).
#'
#' Computes Tjur's (2009) coefficient of determination.
#'
#' @param model Binomial Model.
#'
#' @examples
#' model <- glm(vs ~ wt + mpg, data = mtcars, family = "binomial")
#' performance_R2_tjur(model)
#'
#' @importFrom stats predict residuals
#' @importFrom insight get_response model_info
#'
#' @references Tjur, T. (2009). Coefficients of determination in logistic regression models—A new proposal: The coefficient of discrimination. The American Statistician, 63(4), 366-372.
#'
#' @export
performance_R2_tjur <- function(model) {
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

  D <- abs(m2 - m1)
  # names(D) <- "Tjur's R2"

  return(D)
}