# Should we export this and/or put it in insight (with a wrapper function get_variance?



#' Get fixed effects variance
#' @importFrom stats var
#' @keywords internal
.get_variance_fixed <- function(vals) {
  with(vals, stats::var(as.vector(beta %*% t(X))))
}


#' Compute variance associated with a random-effects term (Johnson 2014)
#' @importFrom stats nobs
#' @keywords internal
.get_variance_random <- function(terms, x, vals) {
  sum(sapply(
    vals$vc[terms],
    function(Sigma) {
      rn <- rownames(Sigma)

      if (!is.null(rn)) {
        valid <- rownames(Sigma) %in% colnames(vals$X)
        if (!all(valid)) {
          rn <- rn[valid]
          Sigma <- Sigma[valid, valid]
        }
      }

      Z <- vals$X[, rn, drop = FALSE]
      # Z <- vals$X[, rownames(Sigma), drop = FALSE]
      Z.m <- Z %*% Sigma
      sum(diag(crossprod(Z.m, Z))) / stats::nobs(x)
    }))
}




#' Get residual (distribution specific) variance from random effects
#' @keywords internal
.get_variance_residual <- function(x, var.cor, faminfo, type) {

  sig <- attr(var.cor, "sc")
  if (is.null(sig)) sig <- 1

  if (faminfo$is_linear) {
    residual.variance <- sig^2
  } else {

    if (faminfo$is_bin) {
      residual.variance <- switch(
        faminfo$link.fun,
        logit = pi^2 / 3,
        probit = 1,
        .badlink(faminfo$link.fun, faminfo$family)
      )
    } else if (faminfo$is_pois) {
      residual.variance <- switch(
        faminfo$link.fun,
        log = .get_variance_distributional(x, null_model(x), faminfo, sig, type = type),
        sqrt = 0.25,
        .badlink(faminfo$link.fun, faminfo$family)
      )
    } else if (faminfo$family == "beta") {
      residual.variance <- switch(
        faminfo$link.fun,
        logit = .get_variance_distributional(x, null_model(x), faminfo, sig, type = type),
        .badlink(faminfo$link.fun, faminfo$family)
      )
    }
  }

  residual.variance
}


#' Get dispersion-specific variance
#' @keywords internal
.get_variance_dispersion <- function(x, vals, faminfo, obs.terms) {
  if (faminfo$is_linear) {
    0
  } else {
    if (length(obs.terms) == 0 )
      0
    else
      .get_variance_random(obs.terms, x = x, vals = vals)
  }
}


#' Get distributional variance for beta-family
#' @keywords internal
.get_variance_beta <- function(mu, phi) {
  mu * (1 - mu) / (1 + phi)
}



# distributional variance for different models
.get_variance_distributional <- function(x, null.fixef, faminfo, sig, type) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Package `lme4` needs to be installed to compute r-squared for mixed models.", call. = FALSE)
  }

  ## in general want log(1+var(x)/mu^2)
  mu <- exp(null.fixef)
  if (mu < 6)
    warning(sprintf("mu of %0.1f is too close to zero, estimate of %s may be unreliable.\n", mu, type), call. = FALSE)

  ## TODO how to get theta or variance from brms-objects?
  cvsquared <- tryCatch(
    {
      vv <- switch(
        faminfo$family,
        poisson = stats::family(x)$variance(mu),
        truncated_poisson = stats::family(x)$variance(sig),
        beta = .get_variance_beta(mu, sig),
        genpois = ,
        nbinom1 = ,
        nbinom2 = stats::family(x)$variance(mu, sig),

        if (inherits(x,"merMod"))
          mu * (1 + mu / lme4::getME(x, "glmer.nb.theta"))
        else
          mu * (1 + mu / x$theta)
      )

      vv / mu^2
    },
    error = function(x) {
      warning("Can't calculate model's distributional variance. Results are not reliable.", call. = F)
      0
    }
  )

  log1p(cvsquared)
}
