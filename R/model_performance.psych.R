#' Performance of FA / PCA models
#'
#' Compute indices of model performance for models from the **psych** package,
#' and for `parameters::factor_analysis()`.
#'
#' @param model A model object of class `fa` (e.g., from `psych::fa()`),
#' `principal` (e.g., from `psych::principal()`), or from
#' `parameters::factor_analysis()`.
#' @param metrics Can be `"all"` or a character vector of metrics to be computed
#' (some of `"Chi2"`, `"Chi2_df"`, `"p_Chi2"`, `"RMSA"`, `"RMSA_corrected"`,
#' `"TLI"`, `"RMSEA"`, `"RMSEA_CI_low"`, `"RMSEA_CI_high"`, and `"BIC"`.
#' @param verbose Toggle off warnings.
#' @param ... Arguments passed to or from other methods.
#'
#' @return A data frame (with one row) and one column per "index" (see
#' `metrics`).
#'
#' @examplesIf all(insight::check_if_installed(c("psych", "GPArotation", "psychTools"), quietly = TRUE))
#' out <- psych::fa(psychTools::bfi[, 1:25], 5)
#' model_performance(out)
#' @export
model_performance.fa <- function(model, metrics = "all", verbose = TRUE, ...) {
  out <- data.frame(
    Chi2 = model$STATISTIC,
    Chi2_df = model$dof,
    p_Chi2 = model$PVAL,
    RMSA = model$rms,
    RMSA_corrected = model$crms,
    TLI = model$TLI,
    RMSEA = model$RMSEA[1],
    RMSEA_CI_low = model$RMSEA[2],
    RMSEA_CI_high = model$RMSEA[3],
    BIC = model$BIC
  )

  if (all(metrics == "all")) {
    metrics <- names(out)
  }
  out <- out[, metrics]

  class(out) <- c("performance_fa", "performance_model", class(out))
  out
}

#' @export
model_performance.principal <- model_performance.fa

#' @export
model_performance.parameters_efa <- function(model, metrics = "all", verbose = TRUE, ...) {
  model_performance(attributes(model)$model, metrics = metrics, verbose = verbose, ...)
}
