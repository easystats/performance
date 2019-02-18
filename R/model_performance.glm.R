#' Performance of General Linear Models
#'
#' Compute indices of model performance for general linear models.
#'
#' @param model Object of class \link{glm}.
#' @param metrics Can be \code{"all"} or a list of metrics to be computed (some of \code{c("AIC", "BIC", "R2")}).
#' @param ... Arguments passed to or from other methods.
#'
#' @examples
#' model <- glm(vs ~ wt + mpg, data = mtcars, family = "binomial")
#' model_performance(model)
#' @importFrom stats AIC BIC
#' @export
model_performance.glm <- function(model, metrics = "all", ...) {


  if (metrics == "all") {
    metrics <- c("AIC", "BIC", "R2")
  }

  out <- list()
  if("AIC" %in% c(metrics)) {
    out$AIC <- AIC(model)
  }
  if("BIC" %in% c(metrics)) {
    out$BIC <- BIC(model)
  }
  if("R2" %in% c(metrics)) {
    out$R2_Tjur <- performance_R2_tjur(model)
  }

  #TODO: What with sigma and deviance?

  out <- as.data.frame(out)
  row.names(out) <- NULL
  out
}