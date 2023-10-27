#' @title Simulate randomized quantile residuals from a model
#' @name simulate_residuals
#'
#' @description to do.
#'
#' @param x A model object.
#' @param ... Arguments passed on to [`DHARMa::simulateResiduals()`].
#'
#' @return Simulated residuals.
#'
#' @details Based on [`DHARMa::simulateResiduals()`]. See also `vignette("DHARMa")`.
#'
#' @references
#'
#' - Hartig, F., & Lohse, L. (2022). DHARMa: Residual Diagnostics for Hierarchical
#'   (Multi-Level / Mixed) Regression Models (Version 0.4.5). Retrieved from
#'   https://CRAN.R-project.org/package=DHARMa
#'
#' - Dunn, P. K., & Smyth, G. K. (1996). Randomized Quantile Residuals. Journal
#'   of Computational and Graphical Statistics, 5(3), 236. \doi{10.2307/1390802}
#'
#' @examples
#' m <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
#' simulate_residuals(m)
#'
#' @export
simulate_residuals <- function(x, ...) {
  insight::check_if_installed("DHARMa")
  # TODO (low priority): Note that DHARMa::simulateResiduals(x, ...) does its own checks for whether
  # or not the model passed to it is supported, do we want to use this or do our
  # own checks so we can supply our own error message?
  #
  # It's important to preserve this object as is, rather than prematurely
  # extracting the residuals from it because the object contains important stuff
  # in it that we'll want to pass onto other functions later, such as passing
  # the fitted model into check_model().
  out <- DHARMa::simulateResiduals(x, ...)
  class(out) <- c("performance_simres", class(out))
  out
}

# methods ------------------------------

#' @export
print.performance_simres <- function(x, ...) {
  # TODO (low priority): We can probably just base this off of the print method
  # DHARMa uses, but with an easystats style. For now we can just stick with
  # DHARMa's method.
  cat(
    "Simulated residuals from a model of class", class(x$fittedModel),
    "based on", x$nSim, "simulations."
  )
}
