#' LOO-related Indices for Bayesian regressions.
#'
#' Compute LOOIC and ELPD for Bayesian regressions.
#'
#' @param model A Bayesian regression model.
#'
#' @examples
#' \dontrun{
#' library(rstanarm)
#'
#' model <- rstanarm::stan_glm(mpg ~ wt + cyl, data = mtcars)
#' performance_LOO(model)
#' }
#'
#'
#' @importFrom stats var
#' @importFrom utils install.packages
#' @export
performance_LOO <- function(model) {

  if (!requireNamespace("loo", quietly = TRUE)) {
    stop("This function needs package `loo` to be installed.")
  }

  out <- list()

  loo_df <- as.data.frame(loo::loo(model)$estimates)

  out$ELPD <- loo_df[rownames(loo_df) == "elpd_loo", ]$Estimate
  out$ELPD_SE <- loo_df[rownames(loo_df) == "elpd_loo", ]$SE
  out$LOOIC <- loo_df[rownames(loo_df) == "looic", ]$Estimate
  out$LOOIC_SE <- loo_df[rownames(loo_df) == "looic", ]$SE

  out <- as.data.frame(out)

  # Leave p_loo as I am not sure it is an index of performance

  return(out)
}