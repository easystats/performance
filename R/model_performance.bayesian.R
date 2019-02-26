#' Performance of Bayesian Models
#'
#' Compute indices of model performance for (general) linear models.
#'
#' @param model Object of class \code{stanreg} or \code{brmsfit}.
#' @param metrics Can be \code{"all"} or a list of metrics to be computed (some of \code{c("LOOIC", "R2", "R2_adj")}).
#' @param ci The Credible Interval level for R2.
#' @param ... Arguments passed to or from other methods.
#'
#' @examples
#' \dontrun{
#' model <- rstanarm::stan_glm(mpg ~ wt + cyl, data = mtcars)
#' model_performance(model)
#' }
#'
#' @seealso \link{r2_bayes}
#' @references Gelman, A., Goodrich, B., Gabry, J., & Vehtari, A. (2018). R-squared for Bayesian regression models. The American Statistician, The American Statistician, 1-6.
#' @importFrom bayestestR hdi
#' @importFrom stats AIC BIC mad median sd setNames
#' @export
model_performance.stanreg <- function(model, metrics = "all", ci = .90, ...) {
  if (metrics == "all") {
    metrics <- c("LOOIC", "R2", "R2_adjusted")
  }

  out <- list()
  if ("LOOIC" %in% c(metrics)) {
    out <- append(out, looic(model))
  }
  if ("R2" %in% c(metrics)) {
    r2 <- .r2_posterior(model)
    out <- c(out, .summarize_r2_bayes(r2$R2_Bayes, ci = ci, name = "R2_"))

    if ("R2_Bayes_fixed" %in% names(r2)) {
      out <- c(out, .summarize_r2_bayes(r2$R2_Bayes_fixed, ci = ci, name = "R2_Fixed_"))
    }
  }
  if ("R2_adjusted" %in% c(metrics)) {
    out$R2_LOO_adjusted <- r2_loo(model)
  }

  # TODO: What with sigma and deviance?

  out <- as.data.frame(out)
  row.names(out) <- NULL
  return(out)
}


#' @export
model_performance.brmsfit <- model_performance.stanreg











#' @keywords internal
.summarize_r2_bayes <- function(r2_posterior, ci = 0.9, name = "R2_") {
  out <- list()
  out$Median <- median(r2_posterior)
  out$MAD <- mad(r2_posterior)
  out$Mean <- mean(r2_posterior)
  out$SD <- sd(r2_posterior)
  out$MAP <- bayestestR::map_estimate(r2_posterior)

  r2_ci <- bayestestR::hdi(r2_posterior, ci = ci)
  if (nrow(r2_ci) > 1) {
    # TODO: as this transformation is also used in parameters, maybe it would be good to put a function in bayestestR
    hdi_low <- as.data.frame(t(setNames(r2_ci$CI_low, as.numeric(r2_ci$CI))))
    names(hdi_low) <- paste0("CI_", names(hdi_low), "_low")
    hdi_high <- as.data.frame(t(setNames(r2_ci$CI_high, as.numeric(r2_ci$CI))))
    names(hdi_high) <- paste0("CI_", names(hdi_high), "_high")
    hdi <- sapply(cbind(hdi_low, hdi_high), as.numeric)
    out <- append(out, as.list(hdi))
  } else {
    out$CI_low <- r2_ci$CI_low
    out$CI_high <- r2_ci$CI_high
  }

  names(out) <- paste0(name, names(out))
  return(out)
}
