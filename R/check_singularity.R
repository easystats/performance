#' @title Check mixed models for boundary fits
#' @name check_singularity
#'
#' @description Check mixed models for boundary fits.
#'
#' @param x A mixed model.
#' @param tolerance Indicates up to which value the convergence result is
#'   accepted. The larger `tolerance` is, the stricter the test
#'   will be.
#' @param check Indicates whether singularity check should be carried out for
#'   the full model (`"model"`, the default), or per random effects term (`"terms"`).
#' @param ... Currently not used.
#'
#' @return `TRUE` if the model fit is singular.
#'
#' @details If a model is "singular", this means that some dimensions of the
#' variance-covariance matrix have been estimated as exactly zero. This
#' often occurs for mixed models with complex random effects structures.
#'
#' "While singular models are statistically well defined (it is theoretically
#' sensible for the true maximum likelihood estimate to correspond to a singular
#' fit), there are real concerns that (1) singular fits correspond to overfitted
#' models that may have poor power; (2) chances of numerical problems and
#' mis-convergence are higher for singular models (e.g. it may be computationally
#' difficult to compute profile confidence intervals for such models); (3)
#' standard inferential procedures such as Wald statistics and likelihood ratio
#' tests may be inappropriate." (_lme4 Reference Manual_)
#'
#' There is no gold-standard about how to deal with singularity and which
#' random-effects specification to choose. Beside using fully Bayesian methods
#' (with informative priors), proposals in a frequentist framework are:
#'
#' - avoid fitting overly complex models, such that the variance-covariance
#'   matrices can be estimated precisely enough (_Matuschek et al. 2017_)
#' - use some form of model selection to choose a model that balances
#'   predictive accuracy and overfitting/type I error (_Bates et al. 2015_,
#'   _Matuschek et al. 2017_)
#' - "keep it maximal", i.e. fit the most complex model consistent with the
#'   experimental design, removing only terms required to allow a non-singular
#'   fit (_Barr et al. 2013_)
#' - since version 1.1.9, the **glmmTMB** package allows to use priors in a
#'   frequentist framework, too. One recommendation is to use a Gamma prior
#'   (_Chung et al. 2013_). The mean may vary from 1 to very large values
#'   (like `1e8`), and the shape parameter should be set to a value of 2.5. You
#'   can then `update()` your model with the specified prior. In **glmmTMB**,
#'   the code would look like this:
#'   ```
#'   # "model" is an object of class gmmmTMB
#'   prior <- data.frame(
#'     prior = "gamma(1, 2.5)",  # mean can be 1, but even 1e8
#'     class = "ranef"           # for random effects
#'   )
#'   model_with_priors <- update(model, priors = prior)
#'   ```
#'   Large values for the mean parameter of the Gamma prior have no large impact
#'   on the random effects variances in terms of a "bias". Thus, if `1` doesn't
#'   fix the singular fit, you can safely try larger values.
#'
#' Note the different meaning between singularity and convergence: singularity
#' indicates an issue with the "true" best estimate, i.e. whether the maximum
#' likelihood estimation for the variance-covariance matrix of the random
#' effects is positive definite or only semi-definite. Convergence is a
#' question of whether we can assume that the numerical optimization has
#' worked correctly or not.
#'
#' @family functions to check model assumptions and and assess model quality
#'
#' @references
#' - Bates D, Kliegl R, Vasishth S, Baayen H. Parsimonious Mixed Models.
#'   arXiv:1506.04967, June 2015.
#'
#' - Barr DJ, Levy R, Scheepers C, Tily HJ. Random effects structure for
#'   confirmatory hypothesis testing: Keep it maximal. Journal of Memory and
#'   Language, 68(3):255-278, April 2013.
#'
#' - Chung Y, Rabe-Hesketh S, Dorie V, Gelman A, and Liu J. 2013. "A Nondegenerate
#'   Penalized Likelihood Estimator for Variance Parameters in Multilevel Models."
#'   Psychometrika 78 (4): 685–709. \doi{10.1007/s11336-013-9328-2}
#'
#' - Matuschek H, Kliegl R, Vasishth S, Baayen H, Bates D. Balancing type I error
#'   and power in linear mixed models. Journal of Memory and Language, 94:305-315, 2017.
#'
#' - lme4 Reference Manual, <https://cran.r-project.org/package=lme4>
#'
#' @examplesIf require("lme4") && require("glmmTMB")
#' data(sleepstudy, package = "lme4")
#' set.seed(123)
#' sleepstudy$mygrp <- sample(1:5, size = 180, replace = TRUE)
#' sleepstudy$mysubgrp <- NA
#' for (i in 1:5) {
#'   filter_group <- sleepstudy$mygrp == i
#'   sleepstudy$mysubgrp[filter_group] <-
#'     sample(1:30, size = sum(filter_group), replace = TRUE)
#' }
#'
#' model <- lme4::lmer(
#'   Reaction ~ Days + (1 | mygrp / mysubgrp) + (1 | Subject),
#'   data = sleepstudy
#' )
#' # any singular fits?
#' check_singularity(model)
#' # singular fit for which particular random effects terms?
#' check_singularity(model, check = "terms")
#'
#' \dontrun{
#' # Fixing singularity issues using priors in glmmTMB
#' # Example taken from `vignette("priors", package = "glmmTMB")`
#' dat <- readRDS(system.file(
#'   "vignette_data",
#'   "gophertortoise.rds",
#'   package = "glmmTMB"
#' ))
#' model <- glmmTMB::glmmTMB(
#'   shells ~ prev + offset(log(Area)) + factor(year) + (1 | Site),
#'   family = poisson,
#'   data = dat
#' )
#' # singular fit
#' check_singularity(model)
#'
#' # impose Gamma prior on random effects parameters
#' prior <- data.frame(
#'   prior = "gamma(1, 2.5)", # mean can be 1, but even 1e8
#'   class = "ranef" # for random effects
#' )
#' model_with_priors <- update(model, priors = prior)
#' # no singular fit
#' check_singularity(model_with_priors)
#' }
#' @export
check_singularity <- function(x, tolerance = 1e-5, ...) {
  UseMethod("check_singularity")
}


#' @export
check_singularity.merMod <- function(x, tolerance = 1e-5, check = "model", ...) {
  insight::check_if_installed(c("lme4", "reformulas"))

  check <- insight::validate_argument(check, c("model", "terms"))
  result <- list()
  vv <- lme4::VarCorr(x)

  re_names <- vapply(
    reformulas::findbars(stats::formula(x)),
    insight::safe_deparse,
    FUN.VALUE = character(1)
  )
  result <- vapply(
    vv,
    function(x) det(x) < tolerance,
    FUN.VALUE = logical(1)
  )

  switch(check,
    model = any(unlist(result, use.names = FALSE)),
    insight::compalist(result)
  )
}

#' @export
check_singularity.rlmerMod <- check_singularity.merMod


#' @rdname check_singularity
#' @export
check_singularity.glmmTMB <- function(x, tolerance = 1e-5, check = "model", ...) {
  insight::check_if_installed(c("lme4", "reformulas"))

  check <- insight::validate_argument(check, c("model", "terms"))
  result <- list()
  vv <- lme4::VarCorr(x)

  for (component in c("cond", "zi", "disp")) {
    re_names <- vapply(
      reformulas::findbars(stats::formula(x, component = component)),
      insight::safe_deparse,
      FUN.VALUE = character(1)
    )
    result[[component]] <- vapply(
      vv[[component]],
      function(x) det(x) < tolerance,
      FUN.VALUE = logical(1)
    )
    names(result[[component]]) <- re_names
  }

  switch(check,
    model = any(unlist(result, use.names = FALSE)),
    insight::compalist(result)
  )
}


#' @export
check_singularity.glmmadmb <- check_singularity.glmmTMB


#' @export
check_singularity.clmm <- function(x, tolerance = 1e-5, ...) {
  insight::check_if_installed("ordinal")

  vc <- ordinal::VarCorr(x)
  any(sapply(vc, function(.x) any(abs(diag(.x)) < tolerance)))
}


#' @export
check_singularity.cpglmm <- function(x, tolerance = 1e-5, ...) {
  insight::check_if_installed("cplm")
  vc <- cplm::VarCorr(x)
  any(sapply(vc, function(.x) any(abs(diag(.x)) < tolerance)))
}


#' @export
check_singularity.MixMod <- function(x, tolerance = 1e-5, ...) {
  any(sapply(diag(x$D), function(.x) any(abs(.x) < tolerance)))
}


#' @export
check_singularity.lme <- function(x, tolerance = 1e-5, ...) {
  insight::check_if_installed("nlme")

  any(abs(stats::na.omit(as.numeric(diag(nlme::getVarCov(x)))) < tolerance))
}


#' @export
check_singularity.default <- function(x, ...) {
  FALSE
}


.collapse_cond <- function(x) {
  if (is.list(x) && "cond" %in% names(x)) {
    x[["cond"]]
  } else {
    x
  }
}
