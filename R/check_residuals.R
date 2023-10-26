#' Check uniformity of simulated residuals
#'
#' `check_residuals()` checks generalized linear (mixed) models for uniformity
#' of randomized quantile residuals, which can be used to identify typical model
#' misspecification problems, such as over/underdispersion, zero-inflation, and
#' residual spatial and temporal autocorrelation.
#'
#' @export
check_residuals <- function(x, ...) {
  # TODO: This should be an S3 method instead of using ifelse
  if (any(class(x) %in% c("performance_simres", "DHARMa"))) {
    # tests if the overall distribution conforms to expectations; equivalent to:
    # ks.test(residuals(simulated_residuals), "punif")
    DHARMa::testUniformity(x, plot = FALSE, ...)
  } else {
    stop("Unsupported input")
  }
}

# methods ------------------------------

# TODO: Add print method
