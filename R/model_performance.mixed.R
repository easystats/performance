#' Performance of Mixed Models
#'
#' Compute indices of model performance for mixed models.
#'
#' @param model Object of class \code{merMod}, \code{glmmTMB}, \code{lme} or \code{MixMod}.
#' @param metrics Can be \code{"all"} or a character vector of metrics to be computed (some of \code{c("AIC", "BIC", "R2", "ICC", "RMSE", "LOGLOSS", "SCORE")}).
#' @param ... Arguments passed to or from other methods.
#'
#' @return A data frame (with one row) and one column per "index" (see \code{metrics}).
#'
#' @details This method returns the \emph{adjusted ICC} only, as this is typically
#'   of interest when judging the variance attributed to the random effects part
#'   of the model (see also \code{\link{icc}}).
#'   \cr \cr
#'   Furthermore, see 'Details' in \code{\link{model_performance.lm}} for
#'   more details on returned indices.
#'
#' @examples
#' library(lme4)
#' model <- lmer(Petal.Length ~ Sepal.Length + (1 | Species), data = iris)
#' model_performance(model)
#'
#' @importFrom insight model_info
#' @importFrom stats AIC BIC
#' @export
model_performance.merMod <- function(model, metrics = "all", ...) {
  if (all(metrics == "all")) {
    metrics <- c("AIC", "BIC", "R2", "ICC", "RMSE", "LOGLOSS", "SCORE")
  }

  mi <- insight::model_info(model)

  out <- list()
  if ("AIC" %in% metrics) {
    out$AIC <- stats::AIC(model)
  }
  if ("BIC" %in% metrics) {
    out$BIC <- stats::BIC(model)
  }
  if ("R2" %in% metrics) {
    rsq <- suppressWarnings(r2(model))
    if (!all(is.na(rsq))) out <- c(out, rsq)
  }
  if ("ICC" %in% metrics) {
    icc_mm <- suppressWarnings(icc(model))
    if (!all(is.na(icc_mm))) out$ICC <- icc_mm$ICC_adjusted
  }
  if ("RMSE" %in% metrics) {
    out$RMSE <- performance_rmse(model)
  }
  if (("LOGLOSS" %in% metrics) && mi$is_binomial) {
    out$LOGLOSS <- performance_logloss(model)
  }
  if (("SCORE" %in% metrics) && (mi$is_binomial || mi$is_count)) {
    .scoring_rules <- performance_score(model)
    if (!is.na(.scoring_rules$logarithmic)) out$SCORE_LOG <- .scoring_rules$logarithmic
    if (!is.na(.scoring_rules$spherical)) out$SCORE_SPHERICAL <- .scoring_rules$spherical
  }

  # TODO: What with sigma and deviance?

  out <- as.data.frame(out)
  row.names(out) <- NULL

  out
}


#' @export
model_performance.lme <- model_performance.merMod

#' @export
model_performance.MixMod <- model_performance.merMod

#' @export
model_performance.mixed <- model_performance.merMod

#' @export
model_performance.glmmTMB <- model_performance.merMod