#' Nakagawa's (2013, 2017) R2 for mixed models
#'
#' @description Compute the marginal and conditional r-squared value for
#'  mixed effects models with complex random effects structures.
#'
#' @param model A mixed effects model of class \code{merMod} or \code{glmmTMB}.
#'
#' @details For mixed models (from \pkg{lme4} and \pkg{glmmTMB}) marginal and
#'  conditional r-squared values are calculated, based on
#'  \cite{Nakagawa et al. 2017}. The distributional variance (or observation-level
#'  variance) that is used to calculate the r-squared is based on lognormal
#'  approximation, \code{log(1+var(x)/mu^2)}. The marginal r-squared considers
#'  only the variance of the fixed effects, while the conditional r-squared
#'  takes both the fixed and random effects into account. The random effect
#'  variances are actually the mean random effect variances, thus the
#'  r-squared value is also appropriate for mixed models with random
#'  slopes or nested random effects (see \cite{Johnson et al. 2014}).
#'
#' @references \itemize{
#'  \item Johnson PC, O'Hara RB. 2014. Extension of Nakagawa & Schielzeth's R2GLMM to random slopes models. Methods Ecol Evol, 5: 944-946. (\doi{10.1111/2041-210X.12225})
#'  \item Nakagawa S, Schielzeth H. 2013. A general and simple method for obtaining R2 from generalized linear mixed-effects models. Methods in Ecology and Evolution, 4(2):133-142. \doi{10.1111/j.2041-210x.2012.00261.x}
#'  \item Nakagawa S, Johnson P, Schielzeth H (2017) The coefficient of determination R2 and intra-class correlation coefficient from generalized linear mixed-effects models revisted and expanded. J. R. Soc. Interface 14. \doi{10.1098/rsif.2017.0213}
#'  }
#'
#' @examples
#' \dontrun{
#' library(lme4)
#' model <- lme4::lmer(Sepal.Length ~ Petal.Length + (1 | Species), data = iris)
#' r2_nakagawa(model)
#' }
#'
#' @export
r2_nakagawa <- function(model) {
  vars <- .compute_variances(model, name = "r2")

  # Calculate R2 values

  r2_marginal <- vars$var.fixef / (vars$var.fixef + vars$var.ranef + vars$var.resid)
  r2_conditional <- (vars$var.fixef + vars$var.ranef) / (vars$var.fixef + vars$var.ranef + vars$var.resid)


  list(
    "R2_conditional" = r2_conditional,
    "R2_marginal" = r2_marginal
  )
}


#' @keywords internal
.compute_variances <- function(model, name = "r2") {
  x <- model

  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Package `lme4` needs to be installed to compute r-squared for mixed models.", call. = FALSE)
  }

  if (is.null(name) || name == "r2") {
    name_fun <- "r2_nakawaga()"
    name_full <- "Nakawaga's R2"
  } else {
    name_fun <- "icc()"
    name_full <- "ICC"
  }



  ## Code taken from GitGub-Repo of package glmmTMB
  ## Author: Ben Bolker, who used an
  ## cleaned-up/adapted version of Jon Lefcheck's code from SEMfit

  faminfo <- insight::model_info(x)

  if (faminfo$family %in% c("truncated_nbinom1", "truncated_nbinom2", "tweedie")) {
    warning(sprintf("Truncated negative binomial and tweedie families are currently not supported by `%s`.", name_fun), call. = F)
    return(NA)
  }

  vals <- list(
    beta = lme4::fixef(x),
    X = lme4::getME(x, "X"),
    vc = lme4::VarCorr(x),
    re = lme4::ranef(x)
  )

  # for glmmTMB, use conditional component of model only,
  # and tell user that zero-inflation is ignored

  if (inherits(x, "glmmTMB")) {
    vals <- lapply(vals, .collapse_cond)

    nullEnv <- function(x) {
      environment(x) <- NULL
      return(x)
    }

    if (!identical(nullEnv(x$modelInfo$allForm$ziformula), nullEnv(~0))) {
      warning(sprintf("%s ignores effects of zero-inflation.", name_fun), call. = FALSE)
    }

    dform <- nullEnv(x$modelInfo$allForm$dispformula)

    if (!identical(dform, nullEnv(~1)) && (!identical(dform, nullEnv(~0)))) {
      warning(sprintf("%s ignores effects of dispersion model.", name_fun), call. = FALSE)
    }
  }


  # Test for non-zero random effects ((near) singularity)

  if (lme4::isSingular(x)) {
    warning(sprintf("Can't compute %s. Some variance components equal zero.\n  Solution: Respecify random structure!", name_full), call. = F)
    return(NA)
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

  if (!all(random.slopes %in% names(vals$beta))) {
    warning(sprintf("Random slopes not present as fixed effects. This artificially inflates the conditional %s.\n  Solution: Respecify fixed structure!", name_full), call. = FALSE)
  }


  # Separate observation variance from variance of random effects

  nr <- sapply(vals$re, nrow)
  not.obs.terms <- names(nr[nr != stats::nobs(x)])
  obs.terms <- names(nr[nr == stats::nobs(x)])


  # Variance of random effects
  var.ranef <- .get_variance_random(not.obs.terms, x = x, vals = vals)

  # Residual variance, which is defined as the variance due to
  # additive dispersion and the distribution-specific variance (Johnson et al. 2014)

  var.dist <- .get_variance_residual(x, var.cor = vals$vc, faminfo, name = name_full)
  var.disp <- .get_variance_dispersion(x = x, vals = vals, faminfo = faminfo, obs.terms = obs.terms)

  var.resid <- var.dist + var.disp

  # save variance information
  #
  # attr(var.measure, "var.fixef") <- var.fixef
  # attr(var.measure, "var.ranef") <- var.ranef
  # attr(var.measure, "var.disp") <- var.disp
  # attr(var.measure, "var.dist") <- var.dist
  # attr(var.measure, "var.resid") <- var.resid
  #
  # attr(var.measure, "family") <- faminfo$family
  # attr(var.measure, "link") <- faminfo$link_function
  # attr(var.measure, "formula") <- insight::find_formula(x)

  # finally, save name of fitted model object. May be needed for
  # the 'se()' function, which accesses the global environment

  # attr(var.measure, ".obj.name") <- obj.name

  return(list(
    "var.fixef" = var.fixef,
    "var.ranef" = var.ranef,
    "var.resid" = var.resid
  ))
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
  if (is.list(fit) && "cond" %in% names(fit)) {
    fit[["cond"]]
  } else {
    fit
  }
}
