#' Performance of instrumental variable regression models
#'
#' @inheritParams model_performance.lm
#' @param metrics Can be \code{"all"}, \code{"common"} or a character vector of metrics to be computed (some of \code{c("AIC", "BIC", "R2", "RMSE", "SIGMA", "Sargan", "Sargan_p", "Wu_Hausman", "Wu_Hausman_p")}). \code{"common"} will compute AIC, BIC, R2 and RMSE.
#' @export
model_performance.ivreg <- function(model, metrics = "all", verbose = TRUE, ...) {

  if (all(metrics == "all")) {
    metrics <- c("AIC", "BIC", "R2", "R2_adj", "RMSE", "SIGMA", "Sargan",
                 "Sargan_p", "Wu_Hausman", "Wu_Hausman_p")
  } else if (all(metrics == "common")) {
    metrics <- c("AIC", "BIC", "R2", "R2_adj", "RMSE")
  }

  out <- model_performance.lm(model, metrics = metrics, verbose = verbose, ...)

  diagnostics <- c("Sargan", "Sargan_p", "Wu_Hausman", "Wu_Hausman_p")
  if (any(metrics %in% diagnostics)) {
    s <- summary(model, diagnostics = TRUE)

    if ("Sargan" %in% metrics) {
      out$Sargan = s$diagnostics["Sargan", "statistic"]
    }

    if ("Sargan_p" %in% metrics) {
      out$Sargan_p = s$diagnostics["Sargan", "p-value"]
    }

    if ("Wu_Hausman" %in% metrics) {
      out$Wu_Hausman = s$diagnostics["Wu-Hausman", "statistic"]
    }

    if ("Wu_Hausman_p" %in% metrics) {
      out$Wu_Hausman_p = s$diagnostics["Wu-Hausman", "p-value"]
    }
  }

  out
}
