#' R2 for Bayesian regressions.
#'
#' Compute R2 for Bayesian models. For mixed models (including a random part), it additionally computes the R2 related to the fixed effects only.
#'
#' @param model A Bayesian regression model.
#'
#' @examples
#' \dontrun{
#' library(rstanarm)
#'
#' model <- rstanarm::stan_glm(mpg ~ wt + cyl, data = mtcars)
#' r2_bayes(model)
#'
#' model <- rstanarm::stan_lmer(Petal.Length ~ Petal.Width + (1|Species), data = iris)
#' r2_bayes(model)
#'
#' library(brms)
#'
#' model <- brms::brm(mpg ~ wt + cyl, data = mtcars)
#' r2_bayes(model)
#'
#' model <- brms::brm(Petal.Length ~ Petal.Width + (1|Species), data = iris)
#' r2_bayes(model)
#' }
#'
#' @references Gelman, A., Goodrich, B., Gabry, J., & Vehtari, A. (2018). R-squared for Bayesian regression models. The American Statistician, The American Statistician, 1-6.
#'
#' @importFrom stats var
#' @importFrom utils install.packages
#' @export
r2_bayes <- function(model) {

  if(!requireNamespace("rstanarm")){
    warning("This function needs `rstanarm` to be installed... installing now.")
    install.packages("rstanarm")
    requireNamespace("rstanarm")
  }

  if(insight::model_info(model)$is_mixed){
    r2_bayesian <- data.frame("R2_Bayes" = as.vector(rstanarm::bayes_R2(model, re.form = NULL, summary=FALSE)),
                              "R2_Bayes_fixed" = as.vector(rstanarm::bayes_R2(model, re.form = NA, summary=FALSE)))
  } else{
    r2_bayesian <- data.frame("R2_Bayes" = as.vector(rstanarm::bayes_R2(model, summary=FALSE)))
  }
  return(r2_bayesian)
}





#' @keywords internal
.summarize_r2_bayes <- function(r2_posterior, ci=0.9, name="R2_"){
  out <- list()
  out$Median <- median(r2_posterior)
  out$MAD <- mad(r2_posterior)
  out$Mean <- mean(r2_posterior)
  out$SD <- sd(r2_posterior)
  out$MAP <- bayestestR::map_estimate(r2_posterior)

  r2_ci <- bayestestR::hdi(r2_posterior, ci=ci)
  if(nrow(r2_ci) > 1){
    # TODO: as this transformation is also used in parameters, maybe it would be good to put a function in bayestestR
    hdi_low <- as.data.frame(t(setNames(r2_ci$CI_low, as.numeric(r2_ci$CI))))
    names(hdi_low) <- paste0("CI_", names(hdi_low), "_low")
    hdi_high <- as.data.frame(t(setNames(r2_ci$CI_high, as.numeric(r2_ci$CI))))
    names(hdi_high) <- paste0("CI_", names(hdi_high), "_high")
    hdi <- sapply(cbind(hdi_low, hdi_high), as.numeric)
    out <- append(out, as.list(hdi))
  } else{
    out$CI_low <- r2_ci$CI_low
    out$CI_high <- r2_ci$CI_high
  }

  names(out) <- paste0(name, names(out))
  return(out)
}