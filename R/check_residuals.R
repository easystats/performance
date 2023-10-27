#' Check uniformity of simulated residuals
#'
#' `check_residuals()` checks generalized linear (mixed) models for uniformity
#' of randomized quantile residuals, which can be used to identify typical model
#' misspecification problems, such as over/underdispersion, zero-inflation, and
#' residual spatial and temporal autocorrelation.
#'
#' @export
check_residuals <- function(x, ...) {
  insight::check_if_installed("DHARMa")
  # TODO: This should be an S3 method instead of using ifelse
  if (inherits(x, c("performance_simres", "DHARMa"))) {
    # tests if the overall distribution conforms to expectations; equivalent to:
    # ks.test(residuals(simulated_residuals), "punif")
    DHARMa::testUniformity(x, plot = FALSE, ...)
  } else {
    insight::format_error("Unsupported input.")
  }
}

# methods ------------------------------

# TODO: Add print method
