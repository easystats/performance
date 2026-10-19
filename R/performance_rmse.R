#' @title Root Mean Squared Error
#' @name performance_rmse
#'
#' @description Compute root mean squared error for (mixed effects) models,
#'   including Bayesian regression models.
#'
#' @param model A model.
#' @param normalized Logical, use `TRUE` if normalized rmse should be returned.
#' @inheritParams icc
#'
#' @details The RMSE is the square root of the variance of the residuals and indicates
#'   the absolute fit of the model to the data (difference between observed data
#'   to model's predicted values). It can be interpreted as the standard
#'   deviation of the unexplained variance, and is in the same units as the
#'   response variable. Lower values indicate better model fit.
#'
#'   The normalized RMSE is the proportion of the RMSE related to the
#'   range of the response variable. Hence, lower values indicate
#'   less residual variance.
#'
#' @return Numeric, the root mean squared error.
#'
#' @examplesIf require("nlme")
#' data(Orthodont, package = "nlme")
#' m <- nlme::lme(distance ~ age, data = Orthodont)
#'
#' # RMSE
#' performance_rmse(m, normalized = FALSE)
#'
#' # normalized RMSE
#' performance_rmse(m, normalized = TRUE)
#' @export
performance_rmse <- function(
  model,
  normalized = FALSE,
  ci = NULL,
  iterations = 100,
  ci_method = NULL,
  verbose = TRUE,
  ...
) {
  tryCatch(
    {
      out <- .calculate_rmse(model, normalized, verbose)
      # check if CIs are requested, and compute CIs
      if (!is.null(ci) && !is.na(ci)) {
        # analytical CI?
        if (identical(ci_method, "analytical")) {
          out <- .analytical_rmse_ci(out, model, ci)
        } else {
          # bootstrapped CI
          result <- .bootstrap_rmse(model, iterations, normalized, ci_method, ...)
          # CI for RMSE
          rmse_ci <- as.vector(result$t[, 1])
          rmse_ci <- rmse_ci[!is.na(rmse_ci)]
          # validation check
          if (length(rmse_ci) > 0) {
            rmse_ci <- bayestestR::eti(rmse_ci, ci = ci)
            out <- cbind(data.frame(RMSE = out), rmse_ci)
            class(out) <- c("performance_rmse", "data.frame")
          } else {
            insight::format_warning("Could not compute confidence intervals for RMSE.")
          }
        }
      }
    },
    error = function(e) {
      if (inherits(e, c("simpleError", "error")) && verbose) {
        insight::print_color(e$message, "red")
        cat("\n")
      }
      out <- NA
    }
  )

  out
}


#' @rdname performance_rmse
#' @export
rmse <- performance_rmse


# methods ---------------------------------------------------------------------

#' @export
format.performance_rmse <- function(x, ...) {
  insight::format_table(x, ...)
}

#' @export
print.performance_rmse <- function(x, ...) {
  cat(insight::export_table(format(x, ...), ...))
}


# helper function to compute RMSE ----------------------------------------------

.calculate_rmse <- function(model, normalized = FALSE, verbose = FALSE, ...) {
  # compute rmse
  rmse_val <- sqrt(performance_mse(model, verbose = verbose))

  # if normalized, divide by range of response
  if (normalized) {
    # get response
    resp <- datawizard::to_numeric(
      insight::get_response(model, verbose = FALSE),
      dummy_factors = FALSE,
      preserve_levels = TRUE
    )
    # compute rmse, normalized
    rmse_val <- rmse_val / (max(resp, na.rm = TRUE) - min(resp, na.rm = TRUE))
  }

  rmse_val
}


# analytical CIs --------------------------------------------------------------

.analytical_rmse_ci <- function(out, model, ci, ...) {
  s <- insight::get_sigma(model, ci = ci, verbose = FALSE)
  n <- insight::n_obs(model)
  conf_ints <- c(attr(s, "CI_low"), attr(s, "CI_high")) * ((n - 1) / n)
  out <- data.frame(
    RMSE = out,
    CI = ci,
    CI_low = conf_ints[1],
    CI_high = conf_ints[2]
  )
  class(out) <- c("performance_rmse", "data.frame")
  out
}


# bootstrapping CIs -----------------------------------------------------------

.boot_calculate_rmse <- function(data, indices, model, normalized, ...) {
  d <- data[indices, ] # allows boot to select sample
  fit <- suppressWarnings(suppressMessages(stats::update(model, data = d)))
  .calculate_rmse(model = fit, normalized = normalized)
}

.bootstrap_rmse <- function(
  model,
  iterations = 100,
  normalized = FALSE,
  ci_method = NULL,
  ...
) {
  if (
    inherits(model, c("merMod", "lmerMod", "glmmTMB")) && !identical(ci_method, "boot")
  ) {
    # cannot pass argument "normalized" to "lme4::bootMer()"
    if (isTRUE(normalized)) {
      insight::format_error(
        "Normalized RMSE cannot be used with confidence intervals. Please use `ci_method = \"boot\"`."
      ) # nolint
    }
    result <- .do_lme4_bootmer(
      model,
      .calculate_rmse,
      iterations,
      dots = list(...)
    )
  } else {
    insight::check_if_installed("boot")
    result <- boot::boot(
      data = insight::get_data(model, verbose = FALSE),
      statistic = .boot_calculate_rmse,
      R = iterations,
      sim = "ordinary",
      model = model,
      normalized = normalized
    )
  }
  result
}
