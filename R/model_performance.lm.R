#' Performance of Regression Models
#'
#' Compute indices of model performance for regression models.
#'
#' @param model A model.
#' @param metrics Can be `"all"`, `"common"` or a character vector of
#'   metrics to be computed (one or more of `"AIC"`, `"AICc"`, `"BIC"`, `"R2"`,
#'   `"R2_adj"`, `"RMSE"`, `"SIGMA"`, `"LOGLOSS"`, `"PCP"`, `"SCORE"`).
#'   `"common"` will compute AIC, BIC, R2 and RMSE.
#' @param verbose Toggle off warnings.
#' @param ... Arguments passed to or from other methods.
#'
#' @return
#' A data frame (with one row) and one column per "index" (see `metrics`).
#'
#' @details Depending on `model`, following indices are computed:
#'
#' - **AIC**: Akaike's Information Criterion, see `?stats::AIC`
#' - **AICc**: Second-order (or small sample) AIC with a correction for small sample sizes
#' - **BIC**: Bayesian Information Criterion, see `?stats::BIC`
#' - **R2**: r-squared value, see [`r2()`]
#' - **R2_adj**: adjusted r-squared, see [`r2()`]
#' - **RMSE**: root mean squared error, see [`performance_rmse()`]
#' - **SIGMA**: residual standard deviation, see [`insight::get_sigma()`]
#' - **LOGLOSS**: Log-loss, see [`performance_logloss()`]
#' - **SCORE_LOG**: score of logarithmic proper scoring rule, see [`performance_score()`]
#' - **SCORE_SPHERICAL**: score of spherical proper scoring rule, see [`performance_score()`]
#' - **PCP**: percentage of correct predictions, see [`performance_pcp()`]
#'
#' @details `model_performance()` correctly detects transformed response and
#' returns the "corrected" AIC and BIC value on the original scale. To get back
#' to the original scale, the likelihood of the model is multiplied by the
#' Jacobian/derivative of the transformation.
#'
#' @examples
#' model <- lm(mpg ~ wt + cyl, data = mtcars)
#' model_performance(model)
#'
#' model <- glm(vs ~ wt + mpg, data = mtcars, family = "binomial")
#' model_performance(model)
#' @export
model_performance.lm <- function(model, metrics = "all", verbose = TRUE, ...) {
  if (any(tolower(metrics) == "log_loss")) {
    metrics[tolower(metrics) == "log_loss"] <- "LOGLOSS"
  }

  # all available options...
  all_metrics <- c("AIC", "AICc", "BIC", "R2", "R2_adj", "RMSE", "SIGMA", "LOGLOSS", "PCP", "SCORE")

  if (all(metrics == "all")) {
    metrics <- all_metrics
  } else if (all(metrics == "common")) {
    metrics <- c("AIC", "BIC", "R2", "R2_adj", "RMSE")
  }

  # check model formula
  if (verbose) {
    insight::formula_ok(model)
  }

  # check for valid input
  metrics <- .check_bad_metrics(metrics, all_metrics, verbose)
  info <- suppressWarnings(insight::model_info(model, verbose = FALSE))

  ## TODO remove is.list() once insight 0.8.3 is on CRAN
  if (is.null(info) || !is.list(info)) {
    info <- list(family = "unknown")
  }

  out <- list()
  attrib <- list()

  # AIC -------------
  if ("AIC" %in% toupper(metrics)) {
    out$AIC <- tryCatch(performance_aic(model, model_info = info),
      error = function(e) NULL
    )
  }

  # AICc -------------
  if ("AICC" %in% toupper(metrics)) {
    out$AICc <- tryCatch(performance_aicc(model),
      error = function(e) NULL
    )
  }

  # BIC -------------
  if ("BIC" %in% toupper(metrics)) {
    out$BIC <- tryCatch(.get_BIC(model),
      error = function(e) NULL
    )
  }

  # R2 -------------
  if (any(c("R2", "R2_ADJ") %in% toupper(metrics))) {
    R2 <- tryCatch(r2(model, verbose = verbose, model_info = info),
      error = function(e) NULL
    )
    if (!is.null(R2)) {
      attrib$r2 <- attributes(R2)
      if ("R2" %in% toupper(metrics) && "R2" %in% names(R2)) {
        out$R2 <- R2$R2
      }
      if ("R2_ADJ" %in% toupper(metrics) && "R2_adjusted" %in% names(R2)) {
        out$R2_adjusted <- R2$R2_adjusted
      }
      if ("R2_ADJ" %in% toupper(metrics) && "R2_adj" %in% names(R2)) {
        out$R2_adjusted <- R2$R2_adj
      }
      if ("R2_within" %in% names(R2)) {
        out$R2_within <- R2$R2_within
      }
      if ("R2_within_adjusted" %in% names(R2)) {
        out$R2_within_adjusted <- R2$R2_within_adjusted
      }
      if (!any(c("R2", "R2_adj", "R2_adjusted", "R2_within", "R2_within_adjusted") %in% names(R2))) {
        out <- c(out, R2)
      }
    }
  }

  # RMSE -------------
  if ("RMSE" %in% toupper(metrics)) {
    out$RMSE <- tryCatch(performance_rmse(model, verbose = verbose),
      error = function(e) NULL
    )
  }

  # SIGMA -------------
  if ("SIGMA" %in% toupper(metrics)) {
    out$Sigma <- tryCatch(.get_sigma(model, verbose = verbose),
      error = function(e) NULL
    )
  }

  # LOGLOSS -------------
  if (("LOGLOSS" %in% toupper(metrics)) && isTRUE(info$is_binomial)) {
    out$Log_loss <- tryCatch(
      {
        .logloss <- performance_logloss(model, verbose = verbose)
        if (!is.na(.logloss)) {
          .logloss
        } else {
          NULL
        }
      },
      error = function(e) {
        NULL
      }
    )
  }

  # SCORE -------------
  if (("SCORE" %in% toupper(metrics)) && (isTRUE(info$is_binomial) || isTRUE(info$is_count))) {
    .scoring_rules <- tryCatch(performance_score(model, verbose = verbose),
      error = function(e) NULL
    )
    if (!is.null(.scoring_rules)) {
      if (!is.na(.scoring_rules$logarithmic)) out$Score_log <- .scoring_rules$logarithmic
      if (!is.na(.scoring_rules$spherical)) out$Score_spherical <- .scoring_rules$spherical
    }
  }

  # PCP -------------
  if (("PCP" %in% toupper(metrics)) &&
    isTRUE(info$is_binomial) &&
    isFALSE(info$is_multinomial) &&
    isFALSE(info$is_ordinal)) {
    out$PCP <- tryCatch(performance_pcp(model, verbose = verbose)$pcp_model,
      error = function(e) NULL
    )
  }


  out <- as.data.frame(insight::compact_list(out, remove_na = TRUE))

  # check if model was actually supported...
  if (nrow(out) == 0 || ncol(out) == 0) {
    if (isTRUE(verbose)) {
      insight::format_warning(paste0("Models of class `", class(model)[1], "` are not yet supported."))
    }
    return(NULL)
  }

  row.names(out) <- NULL
  class(out) <- c("performance_model", class(out))

  # Add attributes
  attributes(out) <- c(attributes(out), attrib)

  out
}

#' @export
model_performance.glm <- model_performance.lm

#' @export
model_performance.Arima <- model_performance.lm

#' @export
model_performance.glmx <- model_performance.lm

#' @export
model_performance.lmrob <- model_performance.lm

#' @export
model_performance.betareg <- model_performance.lm

#' @export
model_performance.censReg <- model_performance.lm

#' @export
model_performance.clm <- model_performance.lm

#' @export
model_performance.clm2 <- model_performance.lm

#' @export
model_performance.coxph <- model_performance.lm

#' @export
model_performance.felm <- model_performance.lm

#' @export
model_performance.iv_robust <- model_performance.lm

#' @export
model_performance.lm_robust <- model_performance.lm

#' @export
model_performance.multinom <- model_performance.lm

#' @export
model_performance.plm <- model_performance.lm

#' @export
model_performance.polr <- model_performance.lm

#' @export
model_performance.bayesx <- model_performance.lm

#' @export
model_performance.survreg <- model_performance.lm

#' @export
model_performance.svyglm <- model_performance.lm

#' @export
model_performance.truncreg <- model_performance.lm

#' @export
model_performance.vglm <- model_performance.lm

#' @export
model_performance.fixest <- model_performance.lm

#' @export
model_performance.DirichletRegModel <- model_performance.lm

#' @export
model_performance.flexsurvreg <- model_performance.lm

#' @export
model_performance.hurdle <- model_performance.lm

#' @export
model_performance.zeroinfl <- model_performance.lm

#' @export
model_performance.zerotrunc <- model_performance.lm





# mfx models -------------------------------

#' @export
model_performance.logitor <- function(model, ...) {
  model_performance(model$fit, ...)
}

#' @export
model_performance.logitmfx <- model_performance.logitor

#' @export
model_performance.probitmfx <- model_performance.logitor

#' @export
model_performance.poissonirr <- model_performance.logitor

#' @export
model_performance.poissonmfx <- model_performance.logitor

#' @export
model_performance.negbinirr <- model_performance.logitor

#' @export
model_performance.negbinmfx <- model_performance.logitor

#' @export
model_performance.betaor <- model_performance.logitor

#' @export
model_performance.betamfx <- model_performance.logitor

#' @export
model_performance.model_fit <- model_performance.logitor





# other models -------------------------------


#' @export
model_performance.mlogit <- function(model, metrics = "all", verbose = TRUE, ...) {
  if (requireNamespace("mlogit", quietly = TRUE)) {
    model_performance.lm(model = model, metrics = metrics, verbose = verbose, ...)
  } else {
    NULL
  }
}


#' @export
model_performance.margins <- function(model, metrics = "all", verbose = TRUE, ...) {
  orig_mod_call <- attributes(model)$call
  model_performance(eval(orig_mod_call), metrics = metrics, verbose = verbose, ...)
}


#' @export
model_performance.sem <- function(model, metrics = "all", verbose = TRUE, ...) {
  if (inherits(model, "sem") && inherits(model, "lme")) {
    model_performance.lm(model, metrics = metrics, verbose = verbose, ...)
  } else {
    NULL
  }
}
