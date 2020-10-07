#' @export
model_performance.bife <- function(model, metrics = "all", verbose = TRUE, ...) {
  if (any(tolower(metrics) == "log_loss")) {
    metrics[tolower(metrics) == "log_loss"] <- "LOGLOSS"
  }

  if (all(metrics == "all")) {
    metrics <- c("AIC", "R2", "LOGLOSS", "PCP")
  } else if (all(metrics == "common")) {
    metrics <- c("AIC", "R2")
  }

  info <- insight::model_info(model)

  out <- list()
  attrib <- list()

  if ("AIC" %in% toupper(metrics)) {
    out$AIC <- performance_aic(model)
  }
  if ("R2" %in% toupper(metrics)) {
    R2 <- r2(model)
    attrib$r2 <- attributes(R2)
    out <- c(out, R2)
  }
  if (("LOGLOSS" %in% toupper(metrics)) && info$is_binomial) {
    .logloss <- performance_logloss(model, verbose = verbose)
    if (!is.na(.logloss)) out$Log_loss <- .logloss
  }
  if (("PCP" %in% toupper(metrics)) && info$is_binomial && !info$is_multinomial && !info$is_ordinal) {
    out$PCP <- performance_pcp(model, verbose = verbose)$pcp_model
  }

  # TODO: What with sigma and deviance?

  out <- as.data.frame(out)
  row.names(out) <- NULL
  class(out) <- c("performance_model", class(out))

  # Add attributes
  attributes(out) <- c(attributes(out), attrib)

  out
}
