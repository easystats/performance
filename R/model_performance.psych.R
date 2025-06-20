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
    Chi2 = ifelse(is.null(model$STATISTIC), NA_real_, model$STATISTIC),
    Chi2_df = ifelse(is.null(model$dof), NA_real_, model$dof),
    p_Chi2 = ifelse(is.null(model$PVAL), NA_real_, model$PVAL),
    RMSA = ifelse(is.null(model$rms), NA_real_, model$rms),
    RMSA_corrected = ifelse(is.null(model$crms), NA_real_, model$crms),
    TLI = ifelse(is.null(model$TLI), NA_real_, model$TLI),
    RMSEA = ifelse(is.null(model$RMSEA), NA_real_, model$RMSEA[1]),
    RMSEA_CI = ifelse(is.null(model$RMSEA), NA_real_, 0.9),
    RMSEA_CI_low = ifelse(is.null(model$RMSEA), NA_real_, model$RMSEA[2]),
    RMSEA_CI_high = ifelse(is.null(model$RMSEA), NA_real_, model$RMSEA[3]),
    BIC = ifelse(is.null(model$BIC), NA_real_, model$BIC)
  )

  if (all(metrics == "all")) {
    metrics <- names(out)
  }

  # clean up
  out <- out[, metrics]
  out <- datawizard::remove_empty_columns(out)

  class(out) <- c("performance_fa", "performance_model", class(out))
  out
}

#' @export
model_performance.principal <- model_performance.fa

#' @export
model_performance.parameters_efa <- function(model, metrics = "all", verbose = TRUE, ...) {
  model_performance(attributes(model)$model, metrics = metrics, verbose = verbose, ...)
}

#' @export
model_performance.omega <- function(model, metrics = "all", verbose = TRUE, ...) {

  class(out) <- c("performance_fa", "performance_model", class(out))
  out
}

#' @export
model_performance.item_omega <- function(model, metrics = "all", verbose = TRUE, ...) {
  x <- attributes(model)$model
  model_performance(x, metrics = metrics, verbose = verbose, ...)
}
