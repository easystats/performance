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
#' @param ... Passed down to [`stats::ks.test()`].
#'
#' @details Uniformity of residuals is checked using a Kolmogorov-Smirnov test.
#' There is a `plot()` method to visualize the distribution of the residuals.
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

#' @rdname check_residuals
#' @export
check_residuals.default <- function(x, alternative = c("two.sided", "less", "greater"), ...) {
  if (insight::is_model(x)) {
    check_residuals(simulate_residuals(x, ...), alternative = alternative)
  } else {
    insight::format_error("`check_residuals()` only works with objects supported by `simulate_residuals()` or `DHARMa::simulateResiduals()`.") # nolint
  }
}

#' @export
check_residuals.performance_simres <- function(x, alternative = c("two.sided", "less", "greater"), ...) {
  alternative <- match.arg(alternative)
  ts_test <- suppressWarnings(
    stats::ks.test(
      stats::residuals(x),
      "punif",
      alternative = alternative,
      ...
    )
  )

  p.val <- ts_test$p.value

  attr(p.val, "data") <- x
  attr(p.val, "object_name") <- insight::safe_deparse_symbol(substitute(x))
  class(p.val) <- unique(c("check_residuals", "see_check_residuals", class(p.val)))

  p.val
}

#' @export
check_residuals.DHARMa <- check_residuals.performance_simres


# methods ------------------------------

#' @export
print.check_residuals <- function(x, ...) {
  pstring <- insight::format_p(x)

  if (x < 0.05) {
    insight::print_color(
      sprintf(
        "Warning: Non-uniformity of simulated residuals detected (%s).\n", pstring
      ),
      "red"
    )
  } else {
    insight::print_color(
      sprintf(
        "OK: Simulated residuals appear as uniformly distributed (%s).\n", pstring
      ),
      "green"
    )
  }

  invisible(x)
}

#' @export
plot.check_residuals <- function(x, ...) {
  insight::check_if_installed("see", "for residual plots")
  NextMethod()
}
