#' Performance of instrumental variable regression models
#'
#' @inheritParams model_performance.lm
#' @param metrics Can be `"all"`, `"common"` or a character vector of
#'   metrics to be computed (some of `c("AIC", "AICc", "BIC", "R2", "RMSE",
#'   "SIGMA", "Sargan", "Wu_Hausman")`). `"common"` will compute AIC, BIC,
#'   R2 and RMSE.
#'
#' @details `model_performance()` correctly detects transformed response and
#' returns the "corrected" AIC and BIC value on the original scale. To get back
#' to the original scale, the likelihood of the model is multiplied by the
#' Jacobian/derivative of the transformation.
#'
#' @export
model_performance.ivreg <- function(model, metrics = "all", verbose = TRUE, ...) {
  all_metrics <- c("AIC", "BIC", "R2", "R2_adj", "RMSE", "SIGMA", "Sargan", "Wu_Hausman")

  if (all(metrics == "all")) {
    metrics <- all_metrics
  } else if (all(metrics == "common")) {
    metrics <- c("AIC", "BIC", "R2", "R2_adj", "RMSE")
  }

  # check for valid input
  metrics <- .check_bad_metrics(metrics, all_metrics, verbose)

  out <- model_performance.lm(model, metrics = metrics, verbose = verbose, ...)

  diagnostics <- c("Sargan", "Wu_Hausman")
  if (any(metrics %in% diagnostics)) {
    s <- summary(model, diagnostics = TRUE)

    if ("Sargan" %in% metrics) {
      out$Sargan <- s$diagnostics["Sargan", "statistic"]
      out$Sargan_p <- s$diagnostics["Sargan", "p-value"]
    }

    if ("Wu_Hausman" %in% metrics) {
      out$Wu_Hausman <- s$diagnostics["Wu-Hausman", "statistic"]
      out$Wu_Hausman_p <- s$diagnostics["Wu-Hausman", "p-value"]
    }

    # remove NA columns
    completed_tests <- intersect(c("Sargan", "Sargan_p", "Wu_Hausman", "Wu_Hausman_p"), colnames(out))
    missing <- sapply(out[completed_tests], function(i) all(is.na(i)))
    if (any(missing)) {
      out[completed_tests[missing]] <- NULL
    }
  }

  out
}
