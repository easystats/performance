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
#'
#' @seealso \link{performance_r2_bayesian}
#' @references Gelman, A., Goodrich, B., Gabry, J., & Vehtari, A. (2018). R-squared for Bayesian regression models. The American Statistician, The American Statistician, 1-6.
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
    r2 <- performance_r2_bayesian(model)
    out <- c(out, .summarize_r2_bayesian(r2$R2_Bayes, ci=ci, name="R2_"))

    if("R2_Bayes_fixed" %in% names(r2)){
      out <- c(out, .summarize_r2_bayesian(r2$R2_Bayes_fixed, ci=ci, name="R2_Fixed_"))
    }
  }
  if("R2_adj" %in% c(metrics)){
    out$R2_LOO_adj <- performance_r2_LOO_adjusted(model)
  }

  #TODO: What with sigma and deviance?

  out <- as.data.frame(out)
  row.names(out) <- NULL
  return(out)
}