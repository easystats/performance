#' @export
model_performance.default <- function(model, metrics = "all", verbose = TRUE, ...) {
  # check for valid input
  .is_model_valid(model)

  if (any(tolower(metrics) == "log_loss")) {
    metrics[tolower(metrics) == "log_loss"] <- "LOGLOSS"
  }

  # all available options...
  all_metrics <- c("AIC", "BIC", "R2", "R2_adj", "RMSE", "SIGMA", "LOGLOSS", "PCP", "SCORE")

  if (all(metrics == "all")) {
    metrics <- all_metrics
  } else if (all(metrics == "common")) {
    metrics <- c("AIC", "BIC", "R2", "R2_adj", "RMSE")
  }

  # check for valid input
  metrics <- .check_bad_metrics(metrics, all_metrics, verbose)

  if (!insight::is_model(model) || !insight::is_model_supported(model)) {
    if (isTRUE(verbose)) {
      warning(paste0("Objects of class '", class(model)[1], "' are not supported model objects."), call. = FALSE)
    }
    return(NULL)
  }

  model_performance.lm(model = model, metrics = metrics, verbose = verbose, ...)
}



.check_bad_metrics <- function(metrics, all_metrics, verbose = TRUE) {
  # check for valid input
  bad_metrics <- which(!metrics %in% all_metrics)
  if (length(bad_metrics)) {
    if (verbose) {
      warning(paste0("Following elements are no valid metric: ",
        metrics[bad_metrics],
        collapse = ", "
      ), call. = FALSE)
    }
    metrics <- metrics[-bad_metrics]
  }
  metrics
}
