#' Performance of Mixed Models
#'
#' Compute indices of model performance for mixed models.
#'
#' @param metrics Can be `"all"`, `"common"` or a character vector of
#'   metrics to be computed (some of `c("AIC", "AICc", "BIC", "R2", "ICC",
#'   "RMSE", "SIGMA", "LOGLOSS", "SCORE")`). `"common"` will compute AIC,
#'   BIC, R2, ICC and RMSE.
#' @param ... Arguments passed to or from other methods.
#' @inheritParams r2_nakagawa
#' @inheritParams model_performance.lm
#' @inheritParams performance_aicc
#'
#' @return A data frame (with one row) and one column per "index" (see
#'   `metrics`).
#'
#' @details
#' \subsection{Intraclass Correlation Coefficient (ICC)}{
#'   This method returns the *adjusted ICC* only, as this is typically of
#'   interest when judging the variance attributed to the random effects part of
#'   the model (see also [icc()]).
#' }
#' \subsection{REML versus ML estimator}{
#'   By default, `estimator = "ML"`, which means that values from information
#'   criteria (AIC, AICc) for specific model classes (like models from *lme4*)
#'   are based on the ML-estimator, while the default behaviour of `AIC()` for
#'   such classes is setting `REML = TRUE`. This default is intentional, because
#'   comparing information criteria based on REML fits is not valid. Set
#'   `estimator = "REML"` explicitly return the same (AIC/...) values as from the
#'   defaults in `AIC.merMod()`.
#' }
#' \subsection{Other performance indices}{
#'   Furthermore, see 'Details' in [model_performance.lm()] for more details
#'   on returned indices.
#' }
#'
#' @examples
#' if (require("lme4")) {
#'   model <- lmer(Petal.Length ~ Sepal.Length + (1 | Species), data = iris)
#'   model_performance(model)
#' }
#' @export
model_performance.merMod <- function(model,
                                     metrics = "all",
                                     estimator = "ML",
                                     verbose = TRUE,
                                     ...) {
  if (any(tolower(metrics) == "log_loss")) {
    metrics[tolower(metrics) == "log_loss"] <- "LOGLOSS"
  }

  # all available metrics
  all_metrics <- c("AIC", "AICc", "BIC", "R2", "ICC", "RMSE", "SIGMA", "LOGLOSS", "SCORE")

  if (all(metrics == "all")) {
    metrics <- all_metrics
  } else if (all(metrics == "common")) {
    metrics <- c("AIC", "BIC", "R2", "ICC", "RMSE")
  }

  # check for valid input
  metrics <- .check_bad_metrics(metrics, all_metrics, verbose)

  # check model formula
  if (verbose) {
    insight::formula_ok(model)
  }

  mi <- insight::model_info(model, verbose = FALSE)

  out <- list()

  if ("AIC" %in% toupper(metrics)) {
    out$AIC <- performance_aic(model, estimator = estimator)
  }

  if ("AICC" %in% toupper(metrics)) {
    out$AICc <- tryCatch(
      {
        performance_aicc(model, estimator = estimator)
      },
      error = function(e) {
        NULL
      }
    )
  }

  if ("BIC" %in% toupper(metrics)) {
    out$BIC <- .get_BIC(model)
  }

  if ("R2" %in% toupper(metrics)) {
    rsq <- suppressWarnings(r2(model))
    if (!all(is.na(rsq))) out <- c(out, rsq)
  }

  if ("ICC" %in% toupper(metrics)) {
    icc_mm <- suppressWarnings(icc(model))
    if (!all(is.na(icc_mm))) out$ICC <- icc_mm$ICC_adjusted
  }

  if ("RMSE" %in% toupper(metrics)) {
    out$RMSE <- performance_rmse(model, verbose = verbose)
  }

  if ("SIGMA" %in% toupper(metrics)) {
    out$Sigma <- .get_sigma(model, verbose = verbose)
  }

  if (("LOGLOSS" %in% toupper(metrics)) && mi$is_binomial) {
    out$Log_loss <- performance_logloss(model, verbose = verbose)
  }

  if (("SCORE" %in% toupper(metrics)) && (mi$is_binomial || mi$is_count)) {
    .scoring_rules <- performance_score(model, verbose = verbose)
    if (!is.na(.scoring_rules$logarithmic)) out$Score_log <- .scoring_rules$logarithmic
    if (!is.na(.scoring_rules$spherical)) out$Score_spherical <- .scoring_rules$spherical
  }

  # TODO: What with sigma and deviance?

  out <- as.data.frame(out)
  row.names(out) <- NULL
  class(out) <- c("performance_model", class(out))

  out
}


#' @export
model_performance.lme <- model_performance.merMod

#' @export
model_performance.glmmadmb <- model_performance.merMod

#' @export
model_performance.rlmerMod <- model_performance.merMod

#' @export
model_performance.MixMod <- model_performance.merMod

#' @export
model_performance.mixed <- model_performance.merMod

#' @export
model_performance.glmmTMB <- model_performance.merMod



#' @export
model_performance.mixor <- function(model,
                                    metrics = "all",
                                    verbose = TRUE,
                                    ...) {
  if (any(tolower(metrics) == "log_loss")) {
    metrics[tolower(metrics) == "log_loss"] <- "LOGLOSS"
  }

  if (all(metrics == "all")) {
    metrics <- c("AIC", "BIC", "LOGLOSS", "SCORE")
  }

  mi <- insight::model_info(model)

  out <- list()
  if ("AIC" %in% metrics) {
    out$AIC <- performance_aic(model)
  }
  if ("BIC" %in% metrics) {
    out$BIC <- .get_BIC(model)
  }
  if (("LOGLOSS" %in% metrics) && mi$is_binomial && !mi$is_ordinal && !mi$is_multinomial) {
    out$Log_loss <- performance_logloss(model, verbose = verbose)
  }
  if (("SCORE" %in% metrics) && (mi$is_binomial || mi$is_count) && !mi$is_ordinal && !mi$is_multinomial) {
    .scoring_rules <- performance_score(model, verbose = verbose)
    if (!is.na(.scoring_rules$logarithmic)) out$Score_log <- .scoring_rules$logarithmic
    if (!is.na(.scoring_rules$spherical)) out$Score_spherical <- .scoring_rules$spherical
  }

  out <- as.data.frame(out)
  row.names(out) <- NULL
  class(out) <- c("performance_model", class(out))

  out
}
