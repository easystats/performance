#' Performance of Linear Models
#'
#' Compute indices of model performance for linear models.
#'
#' @param model Object of class \link{lm}.
#' @param metrics Can be \code{"all"} or a list of metrics to be computed (some of \code{c("AIC", "BIC", "R2", "R2_adj")}).
#' @param ... Arguments passed to or from other methods.
#'
#' @examples
#' model <- lm(mpg ~ wt + cyl, data = mtcars)
#' model_performance(model)
#' @importFrom stats AIC BIC
#' @export
model_performance.lm <- function(model, metrics = "all", ...) {


  if (metrics == "all"){
    metrics <- c("AIC", "BIC", "R2", "R2_adj")
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

  #TODO: What with sigma and deviance?

  out <- as.data.frame(out)
  row.names(out) <- NULL
  out
}