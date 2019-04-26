#' Performance of Bayesian Models
#'
#' Compute indices of model performance for (general) linear models.
#'
#' @param model Object of class \code{stanreg} or \code{brmsfit}.
#' @param metrics Can be \code{"all"} or a character vector of metrics to be computed (some of \code{c("LOOIC", "R2", "R2_adj", "RMSE", "LOGLOSS")}).
#' @param ci The Credible Interval level for R2.
#' @param ... Arguments passed to or from other methods.
#'
#' @return A data frame (with one row) and one column per "index" (see \code{metrics}).
#'
#' @examples
#' library(rstanarm)
#' model <- stan_glm(mpg ~ wt + cyl, data = mtcars, chains = 1, iter = 500)
#' model_performance(model)
#'
#' @seealso \link{r2_bayes}
#' @references Gelman, A., Goodrich, B., Gabry, J., & Vehtari, A. (2018). R-squared for Bayesian regression models. The American Statistician, The American Statistician, 1-6.
#'
#' @importFrom insight find_algorithm
#' @importFrom bayestestR map_estimate hdi
#' @importFrom stats AIC BIC mad median sd setNames
#' @export
model_performance.stanreg <- function(model, metrics = "all", ci = .90, ...) {
  if (all(metrics == "all")) {
    metrics <- c("LOOIC", "R2", "R2_adjusted", "RMSE", "LOGLOSS")
  }

  algorithm <- insight::find_algorithm(model)
  if (algorithm$algorithm != "sampling") {
    warning("`model_performance()` only possible for models fit using the 'sampling' algorithm.", call. = FALSE)
    return(NULL)
  }
  mi <- insight::model_info(model)

  out <- list()
  if ("LOOIC" %in% c(metrics)) {
    out <- append(out, looic(model))
  }
  if ("R2" %in% c(metrics)) {
    r2 <- .r2_posterior(model)
    out <- c(out, .summarize_r2_bayes(r2$R2_Bayes, ci = ci, name = "R2_"))

    if ("R2_Bayes_marginal" %in% names(r2)) {
      out <- c(out, .summarize_r2_bayes(r2$R2_Bayes_marginal, ci = ci, name = "R2_marginal_"))
    }
  }
  if ("R2_adjusted" %in% c(metrics) && mi$is_linear) {
    out$R2_LOO_adjusted <- r2_loo(model)
  }
  if ("RMSE" %in% c(metrics)) {
    out$RMSE <- rmse(model)
  }
  if (("LOGLOSS" %in% metrics) && mi$is_binomial) {
    out$LOGLOSS <- log_loss(model)
  }

  # TODO: What with sigma and deviance?

  out <- as.data.frame(out)
  row.names(out) <- NULL
  out
}


#' @export
model_performance.brmsfit <- model_performance.stanreg


#' @keywords internal
.summarize_r2_bayes <- function(r2_posterior, ci = 0.9, name = "R2_") {
  out <- list()
  out$Median <- stats::median(r2_posterior)
  out$MAD <- stats::mad(r2_posterior)
  out$Mean <- mean(r2_posterior)
  out$SD <- stats::sd(r2_posterior)
  out$MAP <- bayestestR::map_estimate(r2_posterior)

  r2_ci <- bayestestR::hdi(r2_posterior, ci = ci)
  if (nrow(r2_ci) > 1) {
    # TODO: as this transformation is also used in parameters, maybe it would be good to put a function in bayestestR
    hdi_low <- stats::setNames(r2_ci$CI_low, sprintf("CI_%i_low", r2_ci$CI))
    hdi_high <- stats::setNames(r2_ci$CI_high, sprintf("CI_%i_high", r2_ci$CI))
    names(hdi_high) <- paste0("CI_", names(hdi_high), "_high")
    out <- append(out, as.list(c(hdi_low, hdi_high)))
  } else {
    out$CI_low <- r2_ci$CI_low
    out$CI_high <- r2_ci$CI_high
  }

  names(out) <- paste0(name, names(out))
  out
}
