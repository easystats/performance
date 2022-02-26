#' @title LOO-related Indices for Bayesian regressions.
#' @name looic
#'
#' @description Compute LOOIC (leave-one-out cross-validation (LOO) information
#'   criterion) and ELPD (expected log predictive density) for Bayesian
#'   regressions. For LOOIC and ELPD, smaller and larger values are respectively
#'   indicative of a better fit.
#'
#' @param model A Bayesian regression model.
#' @inheritParams model_performance.lm
#'
#' @return A list with four elements, the ELPD, LOOIC and their standard errors.
#'
#' @examples
#' if (require("rstanarm")) {
#'   model <- stan_glm(mpg ~ wt + cyl, data = mtcars, chains = 1, iter = 500, refresh = 0)
#'   looic(model)
#' }
#' @export
looic <- function(model, verbose = TRUE) {
  insight::check_if_installed("loo")

  algorithm <- insight::find_algorithm(model)

  if (algorithm$algorithm != "sampling") {
    if (verbose) {
      warning(insight::format_message("`looic()` only available for models fit using the 'sampling' algorithm."), call. = FALSE)
    }
    return(NA)
  }

  res_loo <- tryCatch(
    {
      loo::loo(model)
    },
    error = function(e) {
      if (inherits(e, c("simpleError", "error"))) {
        insight::print_color(e$message, "red")
        cat("\n")
      }
      NULL
    }
  )
  loo_df <- res_loo$estimates

  if (is.null(loo_df)) {
    return(NULL)
  }

  out <- list(
    ELPD = loo_df["elpd_loo", "Estimate"],
    ELPD_SE = loo_df["elpd_loo", "SE"],
    LOOIC = loo_df["looic", "Estimate"],
    LOOIC_SE = loo_df["looic", "SE"]
  )
  attr(out, "loo") <- res_loo[c("pointwise", "diagnostics")]

  # Leave p_loo as I am not sure it is an index of performance

  structure(class = "looic", out)
}


#' @export
as.data.frame.looic <- function(x, row.names = NULL, ...) {
  data.frame(
    ELPD = x$ELPD,
    ELPD_SE = x$ELPD_SE,
    LOOIC = x$LOOIC,
    LOOIC_SE = x$LOOIC_SE,
    stringsAsFactors = FALSE,
    row.names = row.names,
    ...
  )
}