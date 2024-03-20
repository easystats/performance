#' @title Simulate randomized quantile residuals from a model
#' @name simulate_residuals
#'
#' @description Returns simulated residuals from a model. This is useful for
#' checking the uniformity of residuals, in particular for non-Gaussian models,
#' where the residuals are not expected to be normally distributed.
#'
#' @param x A model object.
#' @param iterations Number of simulations to run.
#' @param ... Arguments passed on to [`DHARMa::simulateResiduals()`].
#' @param object A `performance_simres` object, as returned by `simulate_residuals()`.
#' @param quantile_function A function to apply to the residuals. If `NULL`, the
#' residuals are returned as is. If not `NULL`, the residuals are passed to this
#' function. This is useful for returning normally distributed residuals, for
#' example: `residuals(x, quantile_function = qnorm)`.
#' @param outlier_values A vector of length 2, specifying the values to replace
#' `-Inf` and `Inf` with, respectively.
#'
#' @return Simulated residuals, which can be further processed with
#' [`check_residuals()`]. The returned object is of class `DHARMa` and
#' `performance_simres`.
#'
#' @seealso [`check_residuals()`], [`check_zeroinflation()`],
#' [`check_overdispersion()`] and [`check_predictions()`].
#'
#' @details This function is a small wrapper around [`DHARMa::simulateResiduals()`].
#' It basically only sets `plot = FALSE` and adds an additional class attribute
#' (`"performance_sim_res"`), which allows using the DHARMa object in own plotting
#' functions from the **see** package. See also `vignette("DHARMa")`. There is a
#' `plot()` method to visualize the distribution of the residuals.
#'
#' @section Tests based on simulated residuals:
#' For certain models, resp. model from certain families, tests like
#' [`check_zeroinflation()`] or [`check_overdispersion()`] are based on
#' simulated residuals. These are usually more accurate for such tests than
#' the traditionally used Pearson residuals. However, when simulating from more
#' complex models, such as mixed models or models with zero-inflation, there are
#' several important considerations. `simulate_residuals()` relies on
#' [`DHARMa::simulateResiduals()`], and additional arguments specified in `...`
#' are passed further down to that function. The defaults in DHARMa are set on
#' the most conservative option that works for all models. However, in many
#' cases, the help advises to use different settings in particular situations
#' or for particular models. It is recommended to read the 'Details' in
#' `?DHARMa::simulateResiduals` closely to understand the implications of the
#' simulation process and which arguments should be modified to get the most
#' accurate results.
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
#' # extract residuals
#' head(residuals(simulate_residuals(m)))
#'
#' @export
simulate_residuals <- function(x, iterations = 250, ...) {
  insight::check_if_installed("DHARMa")
  # TODO (low priority): Note that DHARMa::simulateResiduals(x, ...) does its own checks for whether
  # or not the model passed to it is supported, do we want to use this or do our
  # own checks so we can supply our own error message?
  if (iterations < 2) {
    insight::format_error("`iterations` must be at least 2.")
  }
  # It's important to preserve this object as is, rather than prematurely
  # extracting the residuals from it because the object contains important stuff
  # in it that we'll want to pass onto other functions later, such as passing
  # the fitted model into check_model().
  out <- DHARMa::simulateResiduals(x, n = iterations, plot = FALSE, ...)
  class(out) <- c("performance_simres", "see_performance_simres", class(out))
  out
}


# methods ------------------------------

#' @export
print.performance_simres <- function(x, ...) {
  # TODO (low priority): We can probably just base this off of the print method
  # DHARMa uses, but with an easystats style. For now we can just stick with
  # DHARMa's method.
  msg <- paste0(
    "Simulated residuals from a model of class `", class(x$fittedModel)[1],
    "` based on ", x$nSim, " simulations. Use `check_residuals()` to check",
    " uniformity of residuals or `residuals()` to extract simulated residuals.",
    " It is recommended to refer to `?DHARMa::simulateResiudals` and",
    " `vignette(\"DHARMa\")` for more information about different settings",
    " in particular situations or for particular models.\n"
  )
  cat(insight::format_message(msg))
}

#' @export
plot.performance_simres <- function(x, ...) {
  insight::check_if_installed("see", "for residual plots")
  NextMethod()
}


# methods --------------------------

#' @rdname simulate_residuals
#' @export
residuals.performance_simres <- function(object, quantile_function = NULL, outlier_values = NULL, ...) {
  # check for DHARMa argument names
  dots <- list(...)
  if (!is.null(dots$quantileFunction)) {
    quantile_function <- dots$quantileFunction
  }
  if (!is.null(dots$outlierValues)) {
    outlier_values <- dots$outlierValues
  }

  if (is.null(quantile_function)) {
    res <- object$scaledResiduals
  } else {
    res <- quantile_function(object$scaledResiduals)
    if (!is.null(outlier_values)) {
      # check for correct length of outlier_values
      if (length(outlier_values) != 2) {
        insight::format_error("`outlier_values` must be a vector of length 2.")
      }
      res[res == -Inf] <- outlier_values[1]
      res[res == Inf] <- outlier_values[2]
    }
  }
  res
}


# helper functions ---------------------

.simres_statistics <- function(x, statistic_fun, alternative = "two.sided") {
  # summarize the observed and simulated residuals
  if (is.null(statistic_fun)) {
    # we pass the values to compute the p-value directly (for "check_outliers()")
    observed <- x
    simulated <- statistic_fun
  } else {
    # or apply a function to observed and simulated residusls,
    # to calcualte a summary statistic
    observed <- statistic_fun(x$observedResponse)
    simulated <- apply(x$simulatedResponse, 2, statistic_fun)
  }
  # p is simply ratio of simulated zeros to observed zeros
  p <- switch(alternative,
    greater = mean(simulated >= observed),
    less = mean(simulated <= observed),
    min(min(mean(simulated <= observed), mean(simulated >= observed)) * 2, 1)
  )
  list(observed = observed, simulated = simulated, p = p)
}
