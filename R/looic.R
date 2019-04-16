#' @title LOO-related Indices for Bayesian regressions.
#' @name looic
#'
#' @description Compute LOOIC (leave-one-out cross-validation (LOO) information
#'   criterion) and ELPD (expected log predictive density) for Bayesian regressions.
#'
#' @param model A Bayesian regression model.
#'
#' @return A list with four elements, the ELPD, LOOIC and their standard errors.
#'
#' @examples
#' \dontrun{
#' library(rstanarm)
#'
#' model <- rstanarm::stan_glm(mpg ~ wt + cyl, data = mtcars)
#' looic(model)
#' }
#'
#' @importFrom insight find_algorithm print_color
#' @importFrom stats var
#' @export
looic <- function(model) {
  if (!requireNamespace("loo", quietly = TRUE)) {
    stop("This function needs package `loo` to be installed.")
  }

  algorithm <- insight::find_algorithm(model)

  if (algorithm$algorithm != "sampling") {
    warning("`looic()` only available for models fit using the 'sampling' algorithm.", call. = FALSE)
    return(NA)
  }

  out <- list()

  loo_df <- tryCatch(
    {
      as.data.frame(loo::loo(model)$estimates)
    },
    error = function(e) {
      if (inherits(e, c("simpleError", "error"))) {
        insight::print_color(e$message, "red")
        cat("\n")
      }
      NULL
    }
  )

  if (is.null(loo_df)) return(NULL)

  out$ELPD <- loo_df[rownames(loo_df) == "elpd_loo", ]$Estimate
  out$ELPD_SE <- loo_df[rownames(loo_df) == "elpd_loo", ]$SE
  out$LOOIC <- loo_df[rownames(loo_df) == "looic", ]$Estimate
  out$LOOIC_SE <- loo_df[rownames(loo_df) == "looic", ]$SE

  # Leave p_loo as I am not sure it is an index of performance

  structure(
    class = "looic",
    out
  )
}
