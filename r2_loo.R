#' @importFrom insight get_response
#' @importFrom stats var
looR2 <- function(fit) {

  if (!requireNamespace("rstantools", quietly = TRUE))
    stop("Package `rstantools` required. Please install.", call. = FALSE)

  if (!requireNamespace("loo", quietly = TRUE))
    stop("Package `loo` required. Please install.", call. = FALSE)

  y <- insight::get_response(fit)
  ypred <- rstantools::posterior_linpred(fit)


  # for some weird models, not all response values can be
  # predicted, resulting in different lengths between y and ypred

  if (length(y) > ncol(ypred)) {
    tryCatch(
      {
        y <- y[as.numeric(attr(ypred, "dimnames")[[2]])]
      },
      error = function(x) { NULL }
    )
  }

  ll <- rstantools::log_lik(fit)

  r_eff <- loo::relative_eff(
    exp(ll),
    chain_id = rep(1:n_of_chains(fit), each = n_of_samples(fit) / n_of_chains(fit))
  )

  psis_object <- loo::psis(log_ratios = -ll, r_eff = r_eff)
  ypredloo <- loo::E_loo(ypred, psis_object, log_ratios = -ll)$value
  eloo <- ypredloo - y

  1 - stats::var(eloo) / stats::var(y)
}


n_of_chains <- function(x) {
  if (inherits(x, "brmsfit"))
    length(x$fit@stan_args)
  else
    length(x$stanfit@stan_args)
}


n_of_samples <- function(x) {
  if (inherits(x, "brmsfit") && requireNamespace("brms", quietly = TRUE)) {
    brms::nsamples(x, incl_warmup = FALSE)
  } else {
    sum(sapply(x$stanfit@stan_args, function(.x) .x$iter - .x$warmup))
  }
}