#' @title Check model for (non-)normality of residuals.
#' @name check_normality
#'
#' @description Check model for (non-)normality of residuals.
#'
#' @param x A model object.
#' @param ... Currently not used.
#'
#' @return The p-value of the test statistics. A p-value < 0.05 indicates a
#' significant deviation from normal distribution
#'
#' @details \code{check_normality()} calls \code{\link[stats]{shapiro.test}}
#' and checks the standardized residuals for normal distribution. Note that
#' this formal test almost always yields significant results for the distribution
#' of residuals and visual inspection (e.g. qqplots) are preferable.
#'
#' @examples
#' m <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
#' check_normality(m)
#'
#' @importFrom stats shapiro.test rstandard
#' @export
check_normality <- function(x, ...) {
  UseMethod("check_normality")
}


#' @export
check_normality.default <- function(x, ...) {
  # check for normality of residuals
  ts <- stats::shapiro.test(stats::rstandard(x))
  p.val <- ts$p.value

  if (p.val < 0.05) {
    message(sprintf("Non-normality of residuals detected: p = %.3f", p.val))
  } else {
    message("Residuals are normally distributed.")
  }
}
