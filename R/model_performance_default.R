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
    stop(paste0("Objects of class '", class(model)[1], "' are no valid model objects."), call. = FALSE)
  }

  info <- insight::model_info(model)

  ## TODO remove is.list() once insight 0.8.3 is on CRAN
  if (is.null(info) || !is.list(info)) {
    info <- list(family = "unknown")
  }

  out <- list()
  attrib <- list()

  if ("AIC" %in% toupper(metrics)) {
    out$AIC <- tryCatch({
      performance_aic(model)
    },
    error = function(e) {
      NULL
    })
  }
  if ("BIC" %in% toupper(metrics)) {
    out$BIC <- tryCatch({
      .get_BIC(model)
    },
    error = function(e) {
      NULL
    })
  }
  if ("R2" %in% toupper(metrics)) {
    R2 <- tryCatch({
      r2(model)
    },
    error = function(e) {
      NULL
    })
    out <- c(out, R2)
  }
  if ("RMSE" %in% toupper(metrics)) {
    out$RMSE <- tryCatch({
      performance_rmse(model, verbose = verbose)
    },
    error = function(e) {
      NULL
    })
  }
  if ("SIGMA" %in% toupper(metrics)) {
    out$Sigma <- tryCatch({
      .get_sigma(model, verbose = verbose)
    },
    error = function(e) {
      NULL
    })
  }
  if (("LOGLOSS" %in% toupper(metrics)) && isTRUE(info$is_binomial)) {
    out$Log_loss <- tryCatch({
      .logloss <- performance_logloss(model, verbose = verbose)
      if (!is.na(.logloss)) {
        .logloss
      } else {
        NULL
      }
    },
    error = function(e) {
      NULL
    })
  }
  if (("SCORE" %in% toupper(metrics)) && (isTRUE(info$is_binomial) || isTRUE(info$is_count))) {
    out <- tryCatch({
      .scoring_rules <- performance_score(model, verbose = verbose)
      if (!is.na(.scoring_rules$logarithmic)) out$Score_log <- .scoring_rules$logarithmic
      if (!is.na(.scoring_rules$spherical)) out$Score_spherical <- .scoring_rules$spherical
      out
    },
    error = function(e) {
      out
    })
  }
  if (("PCP" %in% toupper(metrics)) && isTRUE(info$is_binomial) && !isTRUE(info$is_multinomial) && !isTRUE(info$is_ordinal)) {
    out$PCP <- tryCatch({
      performance_pcp(model, verbose = verbose)$pcp_model
    },
    error = function(e) {
      NULL
    })
  }

  out <- as.data.frame(.compact_list(out, remove_na = TRUE))

  if (nrow(out) == 0 || ncol(out) == 0) {
    warning(paste0("Models of class '", class(model)[1], "' are not yet supported."), call. = FALSE)
    return(NULL)
  }

  row.names(out) <- NULL
  class(out) <- c("performance_model", class(out))
  out
}
