#' @title Efron's R2
#' @name r2_efron
#'
#' @description Calculates Efron's pseudo R2.
#'
#' @param model Generalized linear model.
#'
#' @return The R2 value.
#'
#' @details
#'
#' Efron's R2 is calculated by taking the sum of the squared model residuals,
#' divided by the total variability in the dependent variable. This R2 equals
#' the squared correlation between the predicted values and actual values,
#' however, note that model residuals from generalized linear models are not
#' generally comparable to those of OLS.
#'
#' @references
#'
#' - Efron, B. (1978). Regression and ANOVA with zero-one data: Measures of
#' residual variation. Journal of the American Statistical Association, 73,
#' 113-121.
#'
#' @examples
#' ## Dobson (1990) Page 93: Randomized Controlled Trial:
#' counts <- c(18, 17, 15, 20, 10, 20, 25, 13, 12) #
#' outcome <- gl(3, 1, 9)
#' treatment <- gl(3, 3)
#' model <- glm(counts ~ outcome + treatment, family = poisson())
#'
#' r2_efron(model)
#' @export
r2_efron <- function(model) {
  UseMethod("r2_efron")
}


#' @export
r2_efron.default <- function(model) {
  .r2_efron(model)
}


.r2_efron <- function(model) {
  y_hat <- stats::predict(model, type = "response")
  y <- datawizard::to_numeric(insight::get_response(model, verbose = FALSE), dummy_factors = FALSE, preserve_levels = TRUE, lowest = 0)
  (1 - (sum((y - y_hat)^2)) / (sum((y - mean(y))^2)))
}
