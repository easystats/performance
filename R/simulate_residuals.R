#' @title Simulate randomized quantile residuals from a model
#' @name simulate_residuals
#'
#' @description to do.
#'
#' @param x A model object.
#' @param iterations Number of simulations to run.
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
#' @examplesIf require("DHARMa")
#' m <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
#' simulate_residuals(m)
#'
#' @export
simulate_residuals <- function(x, iterations = 250, ...) {
  insight::check_if_installed("DHARMa")
  # TODO (low priority): Note that DHARMa::simulateResiduals(x, ...) does its own checks for whether
  # or not the model passed to it is supported, do we want to use this or do our
  # own checks so we can supply our own error message?
  #
  # It's important to preserve this object as is, rather than prematurely
  # extracting the residuals from it because the object contains important stuff
  # in it that we'll want to pass onto other functions later, such as passing
  # the fitted model into check_model().
  out <- DHARMa::simulateResiduals(x, n = iterations, plot = FALSE, ...)
  class(out) <- c("performance_simres", class(out))
  out
}

# methods ------------------------------

#' @export
print.performance_simres <- function(x, ...) {
  # TODO (low priority): We can probably just base this off of the print method
  # DHARMa uses, but with an easystats style. For now we can just stick with
  # DHARMa's method.
  msg <- paste0(
    "Simulated residuals from a model of class `", class(x$fittedModel),
    "` based on ", x$nSim, " simulations. Use `check_residuals()` to check ",
    "uniformity of residuals. It is recommended to refer to `?DHARMa::simulateReisudals`",
    " and `vignette(\"DHARMa\")` for more information about different settings",
    " in particular situations or for particular models."
  )
  cat(insight::format_message(msg))
}
