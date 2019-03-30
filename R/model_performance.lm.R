#' Performance of (Generalized) Linear Models
#'
#' Compute indices of model performance for (generalized) linear models.
#'
#' @param model Object of class \code{lm} or \code{glm}.
#' @param metrics Can be \code{"all"} or a list of metrics to be computed (some of \code{c("AIC", "BIC", "R2", "R2_adj")}).
#' @param ... Arguments passed to or from other methods.
#'
#' @examples
#' model <- lm(mpg ~ wt + cyl, data = mtcars)
#' model_performance(model)
#'
#' model <- glm(vs ~ wt + mpg, data = mtcars, family = "binomial")
#' model_performance(model)
#' @importFrom stats AIC BIC
#' @export
model_performance.lm <- function(model, metrics = c("all", "AIC", "BIC", "R2", "R2_adj"), ...) {
  metrics <- match.arg(metrics)

  if (all(metrics == "all")) {
    metrics <- c("AIC", "BIC", "R2", "R2_adj")
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

  # TODO: What with sigma and deviance?

  out <- as.data.frame(out)
  row.names(out) <- NULL
  out
}

#' @rdname model_performance.lm
#' @export
model_performance.glm <- model_performance.lm
