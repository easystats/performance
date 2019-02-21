#' @title Compute r-squared
#' @name r2
#'
#' @description to do.
#'
#' @param x A fitted model.
#' @param type Character vector, indicating the type of r-squared that is
#'    requested (like \code{"CoxSnell"} or \code{"adjusted"}).
#' @param n Optional, an \code{lme} object, representing the fitted null-model
#'    (unconditional model) to \code{x}. If \code{n} is given, the pseudo-r-squared
#'    for random intercept and random slope variances are computed
#'    (\cite{Kwok et al. 2008}) as well as the Omega squared value
#'    (\cite{Xu 2003}). See 'Examples' and 'Details'.
#' @param loo Logical, if \code{TRUE} and \code{x} is a \code{stanreg} or
#'    \code{brmsfit} object, a LOO-adjusted r-squared is calculated. Else,
#'    a rather "unadjusted" r-squared will be returned by calling
#'    \code{rstantools::bayes_R2()}.
#' @param ... Currently not used.
#'
#' @return The requested r-squared value.
#'
#' @references \itemize{
#'               \item \href{http://glmm.wikidot.com/faq}{DRAFT r-sig-mixed-models FAQ}
#'               \item Bolker B et al. (2017): \href{http://bbolker.github.io/mixedmodels-misc/glmmFAQ.html}{GLMM FAQ.}
#'               \item Byrnes, J. 2008. Re: Coefficient of determination (R^2) when using lme() (\url{https://stat.ethz.ch/pipermail/r-sig-mixed-models/2008q2/000713.html})
#'               \item Kwok OM, Underhill AT, Berry JW, Luo W, Elliott TR, Yoon M. 2008. Analyzing Longitudinal Data with Multilevel Models: An Example with Individuals Living with Lower Extremity Intra-Articular Fractures. Rehabilitation Psychology 53(3): 370–86. \doi{10.1037/a0012765}
#'               \item Nakagawa S, Schielzeth H. 2013. A general and simple method for obtaining R2 from generalized linear mixed-effects models. Methods in Ecology and Evolution, 4(2):133–142. \doi{10.1111/j.2041-210x.2012.00261.x}
#'               \item Nakagawa S, Johnson P, Schielzeth H (2017) The coefficient of determination R2 and intra-class correlation coefficient from generalized linear mixed-effects models revisted and expanded. J. R. Soc. Interface 14. \doi{10.1098/rsif.2017.0213}
#'               \item Rabe-Hesketh S, Skrondal A. 2012. Multilevel and longitudinal modeling using Stata. 3rd ed. College Station, Tex: Stata Press Publication
#'               \item Raudenbush SW, Bryk AS. 2002. Hierarchical linear models: applications and data analysis methods. 2nd ed. Thousand Oaks: Sage Publications
#'               \item Snijders TAB, Bosker RJ. 2012. Multilevel analysis: an introduction to basic and advanced multilevel modeling. 2nd ed. Los Angeles: Sage
#'               \item Xu, R. 2003. Measuring explained variation in linear mixed effects models. Statist. Med. 22:3527-3541. \doi{10.1002/sim.1572}
#'               \item Tjur T. 2009. Coefficients of determination in logistic regression models - a new proposal: The coefficient of discrimination. The American Statistician, 63(4): 366-372
#'             }
#'
#' @details For linear models, the r-squared and adjusted r-squared value is returned,
#'          as provided by the \code{summary}-function.
#'          \cr \cr
#'          For mixed models (from \pkg{lme4} or \pkg{glmmTMB}) marginal and
#'          conditional r-squared values are calculated, based on
#'          \cite{Nakagawa et al. 2017}. The distributional variance
#'          (or observation-level variance) is based on lognormal approximation,
#'          \code{log(1+var(x)/mu^2)}.
#'          \cr \cr
#'          For \code{lme}-models, an r-squared approximation by computing the
#'          correlation between the fitted and observed values, as suggested by
#'          \cite{Byrnes (2008)}, is returned as well as a simplified version of
#'          the Omega-squared value (1 - (residual variance / response variance),
#'          \cite{Xu (2003)}, \cite{Nakagawa, Schielzeth 2013}), unless \code{n}
#'          is specified.
#'          \cr \cr
#'          If \code{n} is given, for \code{lme}-models pseudo r-squared measures based
#'          on the variances of random intercept (tau 00, between-group-variance)
#'          and random slope (tau 11, random-slope-variance), as well as the
#'          r-squared statistics as proposed by \cite{Snijders and Bosker 2012} and
#'          the Omega-squared value (1 - (residual variance full model / residual
#'          variance null model)) as suggested by \cite{Xu (2003)} are returned.
#'          \cr \cr
#'          For generalized linear models, Cox & Snell's and Nagelkerke's
#'          pseudo r-squared values are returned.
#'          \cr \cr
#'          The ("unadjusted") r-squared value and its standard error for
#'          \code{brmsfit} or \code{stanreg} objects are robust measures, i.e.
#'          the median is used to compute r-squared, and the median absolute
#'          deviation as the measure of variability. If \code{loo = TRUE},
#'          a LOO-adjusted r-squared is calculated, which comes conceptionally
#'          closer to an adjusted r-squared measure.
#'
#' @note \describe{
#'         \item{\strong{cod()}}{
#'          This method calculates the Coefficient of Discrimination \code{D}
#'          for generalized linear (mixed) models for binary data. It is
#'          an alternative to other Pseudo-R-squared values like Nakelkerke's
#'          R2 or Cox-Snell R2. The Coefficient of Discrimination \code{D}
#'          can be read like any other (Pseudo-)R-squared value.
#'         }
#'         \item{\strong{r2()}}{
#'          For mixed models, the marginal r-squared considers only the variance
#'          of the fixed effects, while the conditional r-squared takes both
#'          the fixed and random effects into account.
#'          \cr \cr
#'          For \code{lme}-objects, if \code{n} is given, the Pseudo-R2 statistic
#'          is the proportion of explained variance in the random effect after
#'          adding co-variates or predictors to the model, or in short: the
#'          proportion of the explained variance in the random effect of the
#'          full (conditional) model \code{x} compared to the null (unconditional)
#'          model \code{n}.
#'          \cr \cr
#'          The Omega-squared statistics, if \code{n} is given, is 1 - the proportion
#'          of the residual variance of the full model compared to the null model's
#'          residual variance, or in short: the the proportion of the residual
#'          variation explained by the covariates.
#'          \cr \cr
#'          Alternative ways to assess the "goodness-of-fit" is to compare the ICC
#'          of the null model with the ICC of the full model (see \code{\link{icc}}).
#'         }
#'       }
#'
#' @examples
#' data(iris)
#'
#' m <- lm(Sepal.Width ~ Species, data = iris)
#' r2(m)
#'
#' @importFrom stats logLik update
#' @importFrom insight n_obs
#' @export
r2 <- function(x, ...) {
  UseMethod("r2")
}


#' @export
r2.default <- function(x, ...) {
  NULL
}


#' @rdname r2
#' @export
r2.glm <- function(x, type = c("Nagelkerke", "CoxSnell"), ...) {
  type <- match.arg(type)

  ## TODO how to handle poisson?

  n <- insight::n_obs(x)
  CoxSnell <- (1 - exp((x$dev - x$null) / n))

  R2 <- switch(
    type,
    CoxSnell = CoxSnell,
    Nakelkerke = CoxSnell / (1 - exp(-x$null / n))
  )

  ## TODO should we return a named vector?

  names(R2) <- type
  R2
}


#' @export
r2.mlogit <- function(x, ...) {
  R2 <- as.vector(summary(x)$mfR2)
  names(R2) <- "McFadden"
  R2
}


#' @rdname r2
#' @export
r2.lm <- function(x, type = c("adjusted", "unadjusted"), ...) {
  type <- match.arg(type)
  R2 <- switch(
    type,
    adjusted = summary(x)$adj.r.squared,
    unadjusted = summary(x)$r.squared
  )

  names(R2) <- type
  R2
}


#' @export
r2.polr <- function(x, type = c("Nagelkerke", "CoxSnell"), ...) {
  type <- match.arg(type)
  L.base <- stats::logLik(stats::update(x, ~ 1))
  r2glm(x, L.base, type)
}


#' @rdname r2
#' @export
r2.clm2 <- function(x, type = c("Nagelkerke", "CoxSnell"), ...) {
  type <- match.arg(type)
  L.base <- stats::logLik(stats::update(x, location = ~ 1, scale = ~ 1))
  r2glm(x, L.base, type)
}


#' @export
r2.clm <- function(x, type = c("Nagelkerke", "CoxSnell"), ...) {
  type <- match.arg(type)
  L.base <- stats::logLik(stats::update(x, ~ 1))
  r2glm(x, L.base, type)
}


#' @export
r2.vglm <- function(x, type = c("Nagelkerke", "CoxSnell"), ...) {
  if (!(is.null(x@call$summ) && !identical(x@call$summ, 0)))
    stop("Can't get log-likelihood when `summ` is not zero.", call. = FALSE)

  type <- match.arg(type)
  L.base <- stats::logLik(stats::update(x, ~ 1))
  r2glm(x, L.base, type)
}


#' @export
r2.multinom <- function(x, type = c("Nagelkerke", "CoxSnell"), ...) {
  type <- match.arg(type)
  L.base <- stats::logLik(stats::update(x, ~ 1, trace = FALSE))
  r2glm(x, L.base, type)
}


#' @export
r2.plm <- function(x, type = c("adjusted", "unadjusted"), ...) {
  type <- match.arg(type)
  R2 <- switch(
    type,
    adjusted = summary(x)$r.squared[2],
    unadjusted = summary(x)$r.squared[1]
  )

  names(R2) <- type
  R2
}


r2glm <- function(x, L.base, type) {
  L.full <- stats::logLik(x)
  D.full <- -2 * L.full

  D.base <- -2 * L.base
  G2 <- -2 * (L.base - L.full)

  if (inherits(x, c("vglm", "clm2")))
    n <- insight::n_obs(x)
  else
    n <- attr(L.full, "nobs")

  R2 <- switch(
    type,
    CoxSnell = 1 - exp(-G2 / n),
    Nakelkerke = (1 - exp((D.full - D.base) / n)) / (1 - exp(-D.base / n))
  )

  ## TODO should we return a named vector?

  names(R2) <- type
  R2
}


#' @export
r2.merMod <- function(x, ...) {
  r2_mixedmodel(x, type = "r2", obj.name = deparse(substitute(x)))
}


#' @export
r2.glmmTMB <- function(x, ...) {
  r2_mixedmodel(x, type = "r2", obj.name = deparse(substitute(x)))
}


#' @export
r2.brmsfit <- function(x, loo = FALSE, ...) {
  if (!requireNamespace("brms", quietly = TRUE))
    stop("Package `brms` needed for this function to work. Please install it.", call. = FALSE)

  if (isTRUE(loo)) {
    rsq <- looR2(x)
    names(rsq) <- "LOO-adjusted R2"
    rsq
  } else {
    brs <- brms::bayes_R2(x, summary = TRUE, robust = TRUE)
    rsq <- brs[1]
    rsq.se <- brs[2]

    ## TODO how to deal with standard errors / inconsistent return types?

    names(rsq) <- "Bayes R2"
    names(rsq.se) <- "Standard Error"

    list(r2 = rsq, se = rsq.se)
  }
}


#' @export
r2.stanreg <- function(x, loo = FALSE, ...) {
  if (!requireNamespace("rstanarm", quietly = TRUE))
    stop("Package `rstanarm` needed for this function to work. Please install it.", call. = FALSE)

  if (inherits(x, "stanmvreg"))
    return(NULL)

  if (isTRUE(loo)) {
    rsq <- looR2(x)
    names(rsq) <- "LOO-adjusted R2"
    rsq
  } else {
    brs <- rstanarm::bayes_R2(x)
    rsq <- stats::median(brs)
    rsq.se <- stats::mad(brs)

    ## TODO how to deal with standard errors / inconsistent return types?

    names(rsq) <- "Bayes R2"
    names(rsq.se) <- "Standard Error"

    list(r2 = rsq, se = rsq.se)
  }
}


#' @export
r2.stanmvreg <- function(x, ...) {
  NULL
}


#' @export
r2.lme <- function(x, n = NULL, ...) {
  r2linmix(x, n)
}