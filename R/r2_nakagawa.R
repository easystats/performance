#' Nakagawa's (2013, 2018) R2 for mixed models
#'
#' DESCRIPTION TO BE IMPROVED.
#'
#' @param model A statistical model.
#' @param type TODO.
#' @param obj.name TODO.
#' @param fun.type TODO.
#'
#' @importFrom stats nobs
#' @importFrom insight find_formula
#' @export
r2_nakagawa <- function(model, type = NULL, obj.name = NULL, fun.type = NULL) {

  x <- model

  if (!requireNamespace("lme4", quietly = TRUE)) {
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
    vals <- .vals_brms(vals, faminfo)
  }

  # for glmmTMB, use conditional component of model only,
  # and tell user that zero-inflation is ignored

  if (inherits(x,"glmmTMB")) {
    vals <- lapply(vals, .collapse_cond)

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

  var.fixef <- .get_variance_fixed(vals)


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
  var.ranef <- .get_variance_random(not.obs.terms, x = x, vals = vals)

  # Residual variance, which is defined as the variance due to
  # additive dispersion and the distribution-specific variance (Johnson et al. 2014)

  var.dist <- .get_variance_residual(x, var.cor = vals$vc, faminfo, type = ws2)
  var.disp <- .get_variance_dispersion(x = x, vals = vals, faminfo = faminfo, obs.terms = obs.terms)

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








#' helper-function, telling user if model is supported or not
#' @keywords internal
.badlink <- function(link, family) {
  warning(sprintf("Model link '%s' is not yet supported for the %s distribution.", link, family), call. = FALSE)
  return(NA)
}


#' glmmTMB returns a list of model information, one for conditional and one for zero-inflated part, so here we "unlist" it
#' @keywords internal
.collapse_cond <- function(fit) {
  if (is.list(fit) && "cond" %in% names(fit))
    fit[["cond"]]
  else
    fit
}











#' @keywords internal
.vals_brms <- function(vals, faminfo) {
  vals$beta <- vals$beta[, 1]

  vals$re <- lapply(vals$re, function(r) {
    dim.ranef <- dim(r)
    dim.names <- dimnames(r)[[3]]
    v.re <- lapply(1:dim.ranef[3], function(.x) r[1:dim.ranef[1], 1, .x])
    names(v.re) <- dim.names
    as.data.frame(v.re)
  })

  sc <- vals$vc$residual__$sd[1]

  if (.obj_has_name(vals$vc, "residual__"))
    vals$vc <- vals$vc[-which(names(vals$vc) == "residual__")]

  vals$vc <- lapply(vals$vc, function(.x) {
    if (.obj_has_name(.x, "cov")) {
      d <- dim(.x$cov)
      .x <- .x$cov[1:d[1], 1, ]
    } else if (.obj_has_name(.x, "sd")) {
      .x <- .x$sd[1, 1, drop = FALSE]^2
      attr(.x, "dimnames") <- list("Intercept", "Intercept")
    }
    .x
  })
  attr(vals$vc, "sc") <- sc

  # warning: Where does ws come from?
  # if (faminfo$is_zeroinf)
  #   warning(sprintf("%s ignores effects of zero-inflation.", ws), call. = FALSE)

  vals
}
