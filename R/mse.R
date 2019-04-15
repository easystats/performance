#' @title Mean Square Error of Linear Models
#' @name mse
#'
#' @description Compute mean square error of linear models.
#'
#' @inheritParams rmse
#'
#' @details The mean square error is the mean of the sum of squared residuals,
#'    i.e. it measures the average of the squares of the errors. Lower
#'    values (closer to zero) indicate better fit.
#'
#' @return Numeric, the mean square error of \code{model}.
#'
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ hp + gear, data = mtcars)
#' mse(m)
#'
#' @importFrom stats residuals
#' @export
mse <- function(model) {
  mean(stats::residuals(model)^2, na.rm = T)
}
