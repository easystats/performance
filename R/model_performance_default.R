#' @export
model_performance.default <- function(model, metrics = "all", verbose = TRUE, ...) {
  if (any(tolower(metrics) == "log_loss")) {
    metrics[tolower(metrics) == "log_loss"] <- "LOGLOSS"
  }

  if (all(metrics == "all")) {
    metrics <- c("AIC", "BIC", "R2", "R2_adj", "RMSE", "SIGMA", "LOGLOSS", "PCP", "SCORE")
  } else if (all(metrics == "common")) {
    metrics <- c("AIC", "BIC", "R2", "R2_adj", "RMSE")
  }

  if (!insight::is_model(model) || !insight::is_model_supported(model)) {
    if (isTRUE(verbose)) {
      warning(paste0("Objects of class '", class(model)[1], "' are no valid model objects."), call. = FALSE)
    }
    return(NULL)
  }

  model_performance.lm(model = model, metrics = metrics, verbose = verbose, ...)
}




# default methods -----------------------
