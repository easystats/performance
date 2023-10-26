#' @title Check model for (non-)constant error variance
#' @name simulate_residuals
#'
#' @description to do.
#'
#' @param x A model object.
#' @param ... Currently not used.
#'
#' @return Simulated residuals.
#'
#' @details Based on [`DHARMa::simulateResiduals()`].
#'
#' @examples
#' m <<- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
#' check_heteroscedasticity(m)
#'
#' # plot results
#' if (require("see")) {
#'   x <- check_heteroscedasticity(m)
#'   plot(x)
#' }
#' @export
simulate_residuals <- function(x, ...) {
  insight::check_if_installed("DHARMa")
  out <- stats::residuals(DHARMa::simulateResiduals(x, ...))
  class(out) <- c("performance_simres", class(out))
  out
}
