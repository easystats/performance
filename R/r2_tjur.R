#' @title Tjur's R2 - coefficient of determination (D)
#' @name r2_tjur
#'
#' @description This method calculates the Coefficient of Discrimination `D`
#'   (also known as Tjur's R2; \cite{Tjur, 2009}) for generalized linear (mixed) models
#'   for binary outcomes. It is an alternative to other pseudo-R2 values like
#'   Nagelkerke's R2 or Cox-Snell R2. The Coefficient of Discrimination `D`
#'   can be read like any other (pseudo-)R2 value.
#'
#' @param model Binomial Model.
#'
#' @return A named vector with the R2 value.
#'
#' @examples
#' model <- glm(vs ~ wt + mpg, data = mtcars, family = "binomial")
#' r2_tjur(model)
#' @references Tjur, T. (2009). Coefficients of determination in logistic regression models - A new proposal: The coefficient of discrimination. The American Statistician, 63(4), 366-372.
#'
#' @export
r2_tjur <- function(model) {
  # check for valid object class
  if (!insight::model_info(model, verbose = FALSE)$is_binomial) {
    stop("`model` must be binomial.")
  }

  y <- .recode_to_zero(insight::get_response(model, verbose = FALSE))
  pred <- stats::predict(model, type = "response", re.form = NULL)

  # delete pred for cases with missing residuals
  if (anyNA(stats::residuals(model))) {
    pred <- pred[!is.na(stats::residuals(model))]
  }

  categories <- unique(y)
  m1 <- mean(pred[which(y == categories[1])], na.rm = TRUE)
  m2 <- mean(pred[which(y == categories[2])], na.rm = TRUE)

  tjur_d <- abs(m2 - m1)

  names(tjur_d) <- "Tjur's R2"
  tjur_d
}
