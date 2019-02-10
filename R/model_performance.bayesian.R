#' Performance of Bayesian Models
#'
#' Compute indices of model performance for (general) linear models.
#'
#' @param model Object of class \link{lm} or \link{glm}.
#' @param metrics Can be \code{"all"} or a list of metrics to be computed (some of \code{c("LOO", "R2", "R2_adj")}).
#' @param ci The Credible Interval level for R2.
#' @param ... Arguments passed to or from other methods.
#'
#' @examples
#' \dontrun{
#' model <- rstanarm::stan_glm(mpg ~ wt + cyl, data = mtcars)
#' model_performance(model)
#' }
#' @importFrom bayestestR hdi
#' @importFrom stats AIC BIC mad median sd setNames
#' @export
model_performance.stanreg <- function(model, metrics = "all", ci=.90, ...) {


  if (metrics == "all"){
    metrics <- c("LOO", "R2", "R2_adj")
  }

  out <- list()
  if("LOO" %in% c(metrics)){
    out <- append(out, as.list(performance_LOO(model)))
  }
  if("R2" %in% c(metrics)){
    r2 <- performance_R2_bayesian(model)
    out$R2_Median <- median(r2)
    out$R2_MAD <- mad(r2)
    out$R2_Mean <- mean(r2)
    out$R2_SD <- sd(r2)
    r2_ci <- bayestestR::hdi(r2, ci=ci)
    if(nrow(r2_ci) > 1){
      # TODO: as this transformation is also used in parameters, maybe it would be good to put a function in bayestestR
      hdi_low <- as.data.frame(t(setNames(r2_ci$CI_low, as.numeric(r2_ci$CI))))
      names(hdi_low) <- paste0("R2_CI_", names(hdi_low), "_low")
      hdi_high <- as.data.frame(t(setNames(r2_ci$CI_high, as.numeric(r2_ci$CI))))
      names(hdi_high) <- paste0("R2_CI_", names(hdi_high), "_high")
      hdi <- sapply(cbind(hdi_low, hdi_high), as.numeric)
      out <- append(out, as.list(hdi))
    } else{
      out$R2_CI_low <- r2_ci$CI_low
      out$R2_CI_high <- r2_ci$CI_high
    }

  }
  if("R2_adj" %in% c(metrics)){
    out$R2_LOO_adj <- performance_R2_LOO_adjusted(model)
  }

  #TODO: What with sigma and deviance?

  out <- as.data.frame(out)
  row.names(out) <- NULL
  return(out)
}