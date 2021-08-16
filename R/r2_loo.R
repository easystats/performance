#' @title LOO-adjusted R2
#' @name r2_loo
#'
#' @description Compute LOO-adjusted R2.
#'
#' @param model A Bayesian regression model.
#' @inheritParams model_performance.lm
#'
#' @return The LOO-adjusted R2 for `model`, as numeric value.
#'
#' @details Unlike [r2_bayes()], which returns an "unadjusted" R2 value,
#'   `r2_loo()` calculates a LOO-adjusted R2, which comes conceptionally
#'   closer to an "adjusted" R2 measure.
#'
#' @examples
#' if (require("rstanarm")) {
#'   model <- stan_glm(mpg ~ wt + cyl, data = mtcars, chains = 1, iter = 500, refresh = 0)
#'   r2_loo(model)
#' }
#' @export
r2_loo <- function(model, verbose = TRUE) {
  if (inherits(model, "stanmvreg")) {
    if (isTRUE(verbose)) {
      warning("Models of class 'stanmvreg' not yet supported.", call. = FALSE)
      return(NULL)
    }
  }

  insight::check_if_installed("rstantools")
  insight::check_if_installed("loo")

  y <- insight::get_response(model, verbose = FALSE)
  ypred <- rstantools::posterior_linpred(model)


  # for some weird models, not all response values can be
  # predicted, resulting in different lengths between y and ypred

  if (length(y) > ncol(ypred)) {
    tryCatch(
      {
        y <- y[as.numeric(attr(ypred, "dimnames")[[2]])]
      },
      error = function(x) {
        NULL
      }
    )
  }

  ll <- rstantools::log_lik(model)

  algorithm <- insight::find_algorithm(model)
  .n_chains <- algorithm$chains
  .n_samples <- (algorithm$iterations - algorithm$warmup) * algorithm$chains

  r_eff <- loo::relative_eff(
    exp(ll),
    chain_id = rep(1:.n_chains, each = .n_samples / .n_chains)
  )

  psis_object <- loo::psis(log_ratios = -ll, r_eff = r_eff)
  ypredloo <- loo::E_loo(ypred, psis_object, log_ratios = -ll)$value
  eloo <- ypredloo - y

  loor2 <- 1 - stats::var(eloo) / stats::var(y)

  names(loor2) <- "LOO-adjusted R2"
  loor2
}
