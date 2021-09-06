#' @title LOO-adjusted R2
#' @name r2_loo
#'
#' @description Compute LOO-adjusted R2.
#'
#' @param model A Bayesian regression model (from **brms**,
#'   **rstanarm**, **BayesFactor**, etc).
#' @param robust Logical, if `TRUE`, the median instead of mean is used to
#'   calculate the central tendency of the variances.
#' @param ci Value or vector of probability of the CI (between 0 and 1) to be
#'   estimated.
#' @param ... Arguments passed to `r2_posterior()`.
#' @inheritParams model_performance.lm
#'
#' @return A list with the Bayesian R2 value. For mixed models, a list with the
#'   Bayesian R2 value and the marginal Bayesian R2 value. The standard errors
#'   and credible intervals for the R2 values are saved as attributes.
#'
#' @details `r2_loo()` returns an "adjusted" R2 value computed using a
#'   leave-one-out-adjusted posterior distribution. This is conceptually similar
#'   to an adjusted/unbiased R2 estiamte in classical regression modeling. See
#'   [r2_bayes()] for an "unadjusted" R2.
#'   \cr \cr
#'   Mixed models are not currently fully supported.
#'   \cr \cr
#'   `r2_loo_posterior()` is the actual workhorse for `r2_loo()` and
#'   returns a posterior sample of LOO-adjusted Bayesian R2 values.
#'
#' @return A list with the LOO-adjusted R2 value. The standard errors
#'   and credible intervals for the R2 values are saved as attributes.
#'
#' @examples
#' if (require("rstanarm")) {
#'   model <- stan_glm(mpg ~ wt + cyl, data = mtcars, chains = 1, iter = 500, refresh = 0)
#'   r2_loo(model)
#' }
#' @export
r2_loo <- function(model, robust = TRUE, ci = 0.95, verbose = TRUE, ...) {

  loo_r2 <- r2_loo_posterior(model, verbose = verbose, ...)

  if (is.null(loo_r2)) {
    return(NULL)
  }

  loo_r2 <- structure(
    class = "r2_loo",
    lapply(loo_r2, ifelse(robust, stats::median, mean)),
    "SE" = lapply(loo_r2, ifelse(robust, stats::mad, stats::sd)),
    # "Estimates" = lapply(r2_bayesian, bayestestR::point_estimate, centrality = "all", dispersion = TRUE),
    "CI" = lapply(loo_r2, bayestestR::hdi, ci = ci),
    "robust" = robust
  )
  return(loo_r2)
}


#' @export
#' @rdname r2_loo
r2_loo_posterior <- function(model, ...) {
  UseMethod("r2_loo_posterior")
}

#' @export
#' @rdname r2_loo
r2_loo_posterior.brmsfit <- function(model, verbose = TRUE, ...) {
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

  S <- nrow(ypred)
  N <- ncol(ypred)
  rng_state_old <- .Random.seed
  on.exit(assign(".Random.seed", rng_state_old, envir = .GlobalEnv))
  set.seed(model$stanfit@stan_args[[1]]$seed)
  exp_draws <- matrix(rexp(S * N, rate = 1), nrow = S, ncol = N)
  wts <- exp_draws/rowSums(exp_draws)
  var_y <- (
    rowSums(sweep(wts, 2, y^2, FUN = "*")) -
      rowSums(sweep(wts, 2, y, FUN = "*"))^2
  ) * (N/(N - 1))
  var_eloo <- (
    rowSums(sweep(wts, 2, eloo^2, FUN = "*")) -
      rowSums(sweep(wts, 2, eloo, FUN = "*")^2)
  ) * (N/(N - 1))
  loo_r2 <- 1 - var_eloo/var_y
  loo_r2[loo_r2 < -1] <- -1
  loo_r2[loo_r2 > 1] <- 1
  loo_r2 <- list(R2_loo = loo_r2)
  names(loo_r2$R2_loo) <- rep("R2", length(loo_r2$R2_loo))
  return(loo_r2)
}

#' @export
#' @rdname r2_loo
r2_loo_posterior.stanreg <- r2_loo_posterior.brmsfit

#' @export
r2_loo_posterior.stanmvreg <- function(model, verbose = TRUE, ...) {
  if (isTRUE(verbose)) {
    warning("Models of class 'stanmvreg' not yet supported.", call. = FALSE)
  }
  NULL
}

#' @export
r2_loo_posterior.BFBayesFactor <- function(model, verbose = TRUE, ...) {
  if (isTRUE(verbose)) {
    warning("Models of class 'BFBayesFactor' not yet supported.", call. = FALSE)
  }
  NULL
}



#' @export
as.data.frame.r2_loo <- function(x, ...) {
  out <- data.frame(
    R2 = x$R2_loo,
    SD = attributes(x)$SE$R2_loo,
    CI = attributes(x)$CI$R2_loo$CI,
    CI_low = attributes(x)$CI$R2_loo$CI_low,
    CI_high = attributes(x)$CI$R2_loo$CI_high,
    stringsAsFactors = FALSE
  )

  if (!is.null(x$R2_loo_marginal)) {
    out_marginal <- data.frame(
      R2 = x$R2_loo_marginal,
      SD = attributes(x)$SE$R2_loo_marginal,
      CI = attributes(x)$CI$R2_loo_marginal$CI,
      CI_low = attributes(x)$CI$R2_loo_marginal$CI_low,
      CI_high = attributes(x)$CI$R2_loo_marginal$CI_high,
      stringsAsFactors = FALSE
    )

    out$Component <- "conditional"
    out_marginal$Component <- "marginal"
    out <- rbind(out, out_marginal)
  }

  out
}
