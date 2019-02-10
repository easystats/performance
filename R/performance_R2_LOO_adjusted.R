#' LOO-adjusted R2.
#'
#' Compute LOO-adjusted R2.
#'
#' @param model A Bayesian regression model.
#'
#' @examples
#' \dontrun{
#' library(rstanarm)
#'
#' model <- rstanarm::stan_glm(mpg ~ wt + cyl, data = mtcars)
#' performance_R2_LOO_adjusted(model)
#' }
#'
#'
#' @importFrom stats var
#' @importFrom utils install.packages
#' @export
performance_R2_LOO_adjusted <- function(model) {

  if(!requireNamespace("rstantools")){
    warning("This function needs `rstantools` to be installed... installing now.")
    install.packages("rstantools")
    requireNamespace("rstantools")
  }

  if(!requireNamespace("loo")){
    warning("This function needs `loo` to be installed... installing now.")
    install.packages("loo")
    requireNamespace("loo")
  }

  y <- insight::get_response(model)
  ypred <- rstantools::posterior_linpred(model)
  loglikehood <- rstantools::log_lik(model)

  nsamples <- 0
  nchains <- length(model$stanfit@stan_args)
  for (chain in model$stanfit@stan_args) {
    nsamples <- nsamples + (chain$iter - chain$warmup)
  }


  r_eff <- loo::relative_eff(exp(loglikehood),
                             chain_id = rep(1:nchains, each = nsamples / nchains)
  )

  psis_object <- loo::psis(log_ratios = -loglikehood, r_eff = r_eff)
  ypredloo <- loo::E_loo(ypred, psis_object, log_ratios = -loglikehood)$value
  if (length(ypredloo) != length(y)) {
    warning("Something went wrong in the LOO-adjusted R2 computation.")
    return(NA)
  }
  eloo <- ypredloo - y

  R2_LOO_adjusted <- 1 - stats::var(eloo) / stats::var(y)
  return(R2_LOO_adjusted)
}