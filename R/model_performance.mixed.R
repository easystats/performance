#' Performance of Mixed Models
#'
#' Compute indices of model performance for mixed models.
#'
#' @param model Object of class \code{merMod}, \code{glmmTMB}, \code{lme} or \code{MixMod}.
#' @param metrics Can be \code{"all"} or a list of metrics to be computed (some of \code{c("AIC", "BIC", "R2", "ICC")}).
#' @param ... Arguments passed to or from other methods.
#'
#' @examples
#' \dontrun{
#' library(lme4)
#' model <- lmer(Petal.Length ~ Sepal.Length + (1 | Species), data = iris)
#' model_performance(model)
#' }
#'
#' @importFrom stats AIC BIC
#' @export
model_performance.merMod <- function(model, metrics = c("all", "AIC", "BIC", "R2", "ICC"), ...) {
  metrics <- match.arg(metrics)

  if (all(metrics == "all")) {
    metrics <- c("AIC", "BIC", "R2", "ICC")
  }

  out <- list()
  if ("AIC" %in% metrics) {
    out$AIC <- stats::AIC(model)
  }
  if ("BIC" %in% metrics) {
    out$BIC <- stats::BIC(model)
  }
  if ("R2" %in% metrics) {
    out <- c(out, r2(model))
  }
  if ("ICC" %in% metrics) {
    out <- c(out, icc(model))
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
model_performance.glmmTMB <- model_performance.merMod