#' Check uniformity of simulated residuals
#'
#' `check_residuals()` checks generalized linear (mixed) models for uniformity
#' of randomized quantile residuals, which can be used to identify typical model
#' misspecification problems, such as over/underdispersion, zero-inflation, and
#' residual spatial and temporal autocorrelation.
#'
#' @param x An object returned by [`simulate_residuals()`] or
#' [`DHARMa::simulateResiduals()`].
#' @param alternative A character string specifying the alternative hypothesis.
#' See [`stats::ks.test()`] for details.
#' @param ... Passed down to [`stats::ks.test()`]
#'
#' @details Uniformity of residuals is checked using a Kolmogorov-Smirnov test.
#'
#' @seealso [`simulate_residuals()`]
#'
#' @return The p-value of the test statistics.
#'
#' @examplesIf require("DHARMa")
#' dat <- DHARMa::createData(sampleSize = 100, overdispersion = 0.5, family = poisson())
#' m <- glm(observedResponse ~ Environment1, family = poisson(), data = dat)
#' res <- simulate_residuals(m)
#' check_residuals(res)
#'
#' @export
check_residuals <- function(x, ...) {
  UseMethod("check_residuals")
}

#' @export
check_residuals.default <- function(x, ...) {
  insight::format_error("`check_residuals()` only works with objects returned by `simulate_residuals()` by `DHARMa::simulateResiduals()`.") # nolint
}

#' @rdname check_residuals
#' @export
check_residuals.performance_simres <- function(x,
                                               alternative = c("two.sided", "less", "greater"),
                                               ...) {
  alternative <- match.arg(alternative)
  stats::ks.test(
    stats::residuals(simulated_residuals),
    "punif",
    alternative = alternative,
    ...
  )
}

#' @export
check_residuals.DHARMa <- check_residuals.performance_simres


# methods ------------------------------

# TODO: Add print method
