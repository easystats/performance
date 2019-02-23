#' Performance of Mixed Models
#'
#' Compute indices of model performance for mixed models.
#'
#' @param model Object of class \code{merMod}.
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
model_performance.merMod <- function(model, metrics = "all", ...) {
  if (metrics == "all") {
    metrics <- c("AIC", "BIC", "R2", "ICC")
  }

  out <- list()
  if ("AIC" %in% c(metrics)) {
    out$AIC <- AIC(model)
  }
  if ("BIC" %in% c(metrics)) {
    out$BIC <- BIC(model)
  }
  if ("R2" %in% c(metrics)) {
    out <- c(out, r2(model))
  }
  if ("ICC" %in% c(metrics)) {
    out <- c(out, icc(model))
  }

  # TODO: What with sigma and deviance?

  out <- as.data.frame(out)
  row.names(out) <- NULL
  return(out)
}
