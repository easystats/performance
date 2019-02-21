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
model_performance.merMod <- function(model, metrics = "all", ...) {


  if (metrics == "all"){
    metrics <- c("AIC", "BIC")
  }

  out <- list()
  if("AIC" %in% c(metrics)){
    out$AIC <- AIC(model)
  }
  if("BIC" %in% c(metrics)){
    out$BIC <- BIC(model)
  }
  # if("R2" %in% c(metrics)){
  #   model_summary <- summary(model)
  #   out$R2 <- model_summary$r.squared
  #
  #   out$`F` <- model_summary$fstatistic[1]
  #   out$DoF <- model_summary$fstatistic[2]
  #   out$DoF_residual <- model_summary$fstatistic[3]
  #   out$p <- stats::pf(
  #     out$`F`,
  #     out$DoF,
  #     out$DoF_residual,
  #     lower.tail = FALSE)
  #
  #   out$R2_adj <- model_summary$adj.r.squared
  # }

  #TODO: What with sigma and deviance?

  out <- as.data.frame(out)
  row.names(out) <- NULL
  return(out)
}