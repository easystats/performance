#' @title Residual Standard Error for Linear Models
#' @name performance_rse
#'
#' @description Compute residual standard error of linear models.
#'
#' @inheritParams performance_rmse
#'
#' @details The residual standard error is the square root of the residual
#'   sum of squares divided by the residual degrees of freedom.
#'
#' @return Numeric, the residual standard error of `model`.
#'
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ hp + gear, data = mtcars)
#' performance_rse(m)
#' @export
performance_rse <- function(model) {
  # Residual standard error
  sqrt(sum(insight::get_residuals(model)^2, na.rm = TRUE) / insight::get_df(model, type = "residual"))
}
