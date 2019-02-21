#' @importFrom stats nobs
#' @importFrom insight find_formula
r2_mixedmodel <- function(x, type = NULL, obj.name = NULL, fun.type = NULL) {

  if (!requireNamespace("lme", quietly = TRUE)) {
    stop("Package `lme4` needs to be installed to compute r-squared for mixed models.", call. = FALSE)
  }

  if (is.null(fun.type) || fun.type == "r2") {
    ws <- "r2()"
    ws2 <- "R2"
    fun.type <- "r2"
  } else {
    ws <- "icc()"
    ws2 <- "ICC"
    fun.type <- "icc"
  }



  ## Code taken from GitGub-Repo of package glmmTMB
  ## Author: Ben Bolker, who used an
  ## cleaned-up/adapted version of Jon Lefcheck's code from SEMfit

  faminfo <- insight::model_info(x)

  if (faminfo$family %in% c("truncated_nbinom1", "truncated_nbinom2", "tweedie")) {
    warning(sprintf("Truncated negative binomial and tweedie families are currently not supported by `%s`.", ws), call. = F)
    return(NULL)
  }

  vals <- list(
    beta = lme4::fixef(x),
    X = lme4::getME(x, "X"),
    vc = lme4::VarCorr(x),
    re = lme4::ranef(x)
  )

  # fix brms structure
  if (inherits(x, "brmsfit")) {
    vals <- vals_brms(vals, faminfo)
  }

  # for glmmTMB, use conditional component of model only,
  # and tell user that zero-inflation is ignored

  if (inherits(x,"glmmTMB")) {
    vals <- lapply(vals, collapse_cond)

    nullEnv <- function(x) {
      environment(x) <- NULL
      return(x)
    }

    if (!identical(nullEnv(x$modelInfo$allForm$ziformula), nullEnv(~0)))
      warning(sprintf("%s ignores effects of zero-inflation.", ws), call. = FALSE)

    dform <- nullEnv(x$modelInfo$allForm$dispformula)

    if (!identical(dform,nullEnv(~1)) && (!identical(dform, nullEnv(~0))))
      warning(sprintf("%s ignores effects of dispersion model.", ws), call. = FALSE)
  }


  # Test for non-zero random effects ((near) singularity)

  if (lme4::isSingular(x)) {
    warning(sprintf("Can't compute %s. Some variance components equal zero.\n  Solution: Respecify random structure!", ws2), call. = F)
    return(NULL)
  }


  # Get variance of fixed effects: multiply coefs by design matrix

  var.fixef <- get_fixef_variance(vals)


  # Are random slopes present as fixed effects? Warn.

  random.slopes <- if ("list" %in% class(vals$re)) {
    # multiple RE
    unique(c(sapply(vals$re, colnames)))
  } else if (is.list(vals$re)) {
    colnames(vals$re[[1]])
  } else {
    colnames(vals$re)
  }

  if (!all(random.slopes %in% names(vals$beta)))
    warning(sprintf("Random slopes not present as fixed effects. This artificially inflates the conditional %s.\n  Solution: Respecify fixed structure!", ws2), call. = FALSE)


  # Separate observation variance from variance of random effects

  nr <- sapply(vals$re, nrow)
  not.obs.terms <- names(nr[nr != stats::nobs(x)])
  obs.terms <- names(nr[nr == stats::nobs(x)])


  # Variance of random effects
  var.ranef <- get_ranef_variance(not.obs.terms, x = x, vals = vals)

  # Residual variance, which is defined as the variance due to
  # additive dispersion and the distribution-specific variance (Johnson et al. 2014)

  var.dist <- get_residual_variance(x, var.cor = vals$vc, faminfo, type = ws2)
  var.disp <- get_disp_variance(x = x, vals = vals, faminfo = faminfo, obs.terms = obs.terms)

  var.resid <- var.dist + var.disp


  # Calculate R2 values

  rsq.marginal <- var.fixef / (var.fixef + var.ranef + var.resid)
  rsq.conditional <- (var.fixef + var.ranef) / (var.fixef + var.ranef + var.resid)

  names(rsq.marginal) <- "Marginal R2"
  names(rsq.conditional) <- "Conditional R2"


  # Calculate ICC values

  icc.adjusted <- var.ranef / (var.ranef + var.resid)
  icc.conditional <- var.ranef / (var.fixef + var.ranef + var.resid)

  names(icc.adjusted) <-    "Adjusted ICC"
  names(icc.conditional) <- "Conditional ICC"

  var.measure <- switch(
    fun.type,
    r2 = {
      if (type == "marginal")
        rsq.marginal
      else
        rsq.conditional
    },
    icc = {
      if (type == "adjusted")
        icc.adjusted
      else
        icc.conditional
    }
  )

  # save variance information

  attr(var.measure, "var.fixef") <- var.fixef
  attr(var.measure, "var.ranef") <- var.ranef
  attr(var.measure, "var.disp") <- var.disp
  attr(var.measure, "var.dist") <- var.dist
  attr(var.measure, "var.resid") <- var.resid

  attr(var.measure, "family") <- faminfo$family
  attr(var.measure, "link") <- faminfo$link_function
  attr(var.measure, "formula") <- insight::find_formula(x)

  # finally, save name of fitted model object. May be needed for
  # the 'se()' function, which accesses the global environment

  attr(var.measure, ".obj.name") <- obj.name

  var.measure
}



# Compute variance associated with a random-effects term
# (Johnson 2014)

get_ranef_variance <- function(terms, x, vals) {
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


# get fixed effects variance

#' @importFrom stats var
get_fixef_variance <- function(vals) {
  with(vals, stats::var(as.vector(beta %*% t(X))))
}


# Get residual (distribution specific) variance from random effects

get_residual_variance <- function(x, var.cor, faminfo, type) {

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
        badlink(faminfo$link.fun, faminfo$family)
      )
    } else if (faminfo$is_pois) {
      residual.variance <- switch(
        faminfo$link.fun,
        log = logVarDist(x, null_model(x), faminfo, sig, type = type),
        sqrt = 0.25,
        badlink(faminfo$link.fun, faminfo$family)
      )
    } else if (faminfo$family == "beta") {
      residual.variance <- switch(
        faminfo$link.fun,
        logit = logVarDist(x, null_model(x), faminfo, sig, type = type),
        badlink(faminfo$link.fun, faminfo$family)
      )
    }
  }

  residual.variance
}


# get dispersion-specific variance

get_disp_variance <- function(x, vals, faminfo, obs.terms) {
  if (faminfo$is_linear) {
    0
  } else {
    if (length(obs.terms) == 0 )
      0
    else
      get_ranef_variance(obs.terms, x = x, vals = vals)
  }
}


# helper-function, telling user if model is supported or not

badlink <- function(link, family) {
  warning(sprintf("Model link '%s' is not yet supported for the %s distribution.", link, family), call. = FALSE)
  return(NA)
}


# glmmTMB returns a list of model information, one for conditional and one
# for zero-inflated part, so here we "unlist" it

collapse_cond <- function(fit) {
  if (is.list(fit) && "cond" %in% names(fit))
    fit[["cond"]]
  else
    fit
}


# Generate null model (intercept and random effects only, no fixed effects)

#' @importFrom stats formula reformulate update
null_model <- function(x) {
  if (!requireNamespace("lme", quietly = TRUE)) {
    stop("Package `lme4` needs to be installed to compute r-squared for mixed models.", call. = FALSE)
  }

  # yet another brms fix
  f <- stats::formula(x)

  if (is.list(f) && obj_has_name(f, "formula")) f <- f$formula

  ## https://stat.ethz.ch/pipermail/r-sig-mixed-models/2014q4/023013.html
  rterms <- paste0("(", sapply(lme4::findbars(f), deparse, width.cutoff = 500), ")")
  nullform <- stats::reformulate(rterms, response = ".")
  null.model <- stats::update(x, nullform)

  ## Get the fixed effects of the null model
  unname(collapse_cond(lme4::fixef(null.model)))
}


# helper function, compute distributional variance for beta-family

beta_variance <- function(mu, phi) {
  mu * (1 - mu) / (1 + phi)
}


# distributional variance for different models

logVarDist <- function(x, null.fixef, faminfo, sig, type) {
  if (!requireNamespace("lme", quietly = TRUE)) {
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
        beta = beta_variance(mu, sig),
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


vals_brms <- function(vals, faminfo) {
  vals$beta <- vals$beta[, 1]

  vals$re <- lapply(vals$re, function(r) {
    dim.ranef <- dim(r)
    dim.names <- dimnames(r)[[3]]
    v.re <- lapply(1:dim.ranef[3], function(.x) r[1:dim.ranef[1], 1, .x])
    names(v.re) <- dim.names
    as.data.frame(v.re)
  })

  sc <- vals$vc$residual__$sd[1]

  if (obj_has_name(vals$vc, "residual__"))
    vals$vc <- vals$vc[-which(names(vals$vc) == "residual__")]

  vals$vc <- lapply(vals$vc, function(.x) {
    if (obj_has_name(.x, "cov")) {
      d <- dim(.x$cov)
      .x <- .x$cov[1:d[1], 1, ]
    } else if (obj_has_name(.x, "sd")) {
      .x <- .x$sd[1, 1, drop = FALSE]^2
      attr(.x, "dimnames") <- list("Intercept", "Intercept")
    }
    .x
  })
  attr(vals$vc, "sc") <- sc

  if (faminfo$is_zeroinf)
    warning(sprintf("%s ignores effects of zero-inflation.", ws), call. = FALSE)

  vals
}
