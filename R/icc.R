#' The Intraclass Correlation Coefficient (ICC) for mixed models
#'
#' @description This function calculates the intraclass-correlation
#'  (icc) - sometimes also called \emph{variance partition coefficient}
#'  (vpc) - for mixed effects models. The ICC is calculated for \code{\link[lme4]{merMod}},
#'  \code{\link[glmmTMB]{glmmTMB}}, \code{MixMod}, \code{lme}, and
#'  \code{stanreg} objects and can be interpreted as \dQuote{the proportion
#'  of the variance explained by the grouping structure in the population}
#'  \cite{(Hox 2002: 15)}. For models fitted with the \pkg{brms}-package,
#'  a variance decomposition based on the posterior predictive distribution
#'  is calculated (see 'Details').
#'
#' @param model A mixed effects model of class \code{merMod}, \code{glmmTMB},
#'  \code{MixMod}, \code{lme}, \code{stanreg} or \code{brmsfit}.
#' @param re.form Formula containing group-level effects to be considered in
#'   the prediction. If \code{NULL} (default), include all group-level effects.
#'   Else, for instance for nested models, name a specific group-level effect
#'   to calculate the variance decomposition for this group-level.
#' @param ci The Credible Interval level.
#' @param ... Currently not used.
#'
#' @inheritParams r2_bayes
#'
#' @references \itemize{
#'  \item Hox J. 2002. Multilevel analysis: techniques and applications. Mahwah, NJ: Erlbaum
#'  \item Johnson PC, O'Hara RB. 2014. Extension of Nakagawa & Schielzeth's R2GLMM to random slopes models. Methods Ecol Evol, 5: 944-946. (\doi{10.1111/2041-210X.12225})
#'  \item Nakagawa S, Johnson P, Schielzeth H (2017) The coefficient of determination R2 and intra-class correlation coefficient from generalized linear mixed-effects models revisted and expanded. J. R. Soc. Interface 14. \doi{10.1098/rsif.2017.0213}
#'  \item Rabe-Hesketh S, Skrondal A. 2012. Multilevel and longitudinal modeling using Stata. 3rd ed. College Station, Tex: Stata Press Publication
#'  \item Raudenbush SW, Bryk AS. 2002. Hierarchical linear models: applications and data analysis methods. 2nd ed. Thousand Oaks: Sage Publications
#'  }
#'
#' @details
#'  \strong{Adjusted and conditional ICC}
#'  \cr \cr
#'  \code{icc()} calculates an adjusted and conditional ICC, which take
#'  all sources of uncertainty (i.e. of \emph{all random effects}) into account. While
#'  the adjusted ICC only relates to the random effects, the conditional ICC
#'  also takes the fixed effects variances into account (see \cite{Nakagawa et al. 2017}).
#'  Typically, the \emph{adjusted} ICC is of interest when the analysis of random
#'  effects is of interest.
#'  \code{icc()} return a meaningful ICC also for models with random slopes and
#'  is applicable for models with other distributions than Gaussian.
#'  \cr \cr
#'  \strong{ICC for unconditional and conditional models}
#'  \cr \cr
#'  Usually, the ICC is calculated for the null model ("unconditional model").
#'  However, according to \cite{Raudenbush and Bryk (2002)} or
#'  \cite{Rabe-Hesketh and Skrondal (2012)} it is also feasible to compute the ICC
#'  for full models with covariates ("conditional models") and compare how
#'  much, e.g., a level-2 variable explains the portion of variation in the grouping
#'  structure (random intercept).
#'  \cr \cr
#'  \strong{ICC for brms-models}
#'  \cr \cr
#'  If \code{model} is of class \code{brmsfit}, \code{icc()} calculates a
#'  variance decomposition based on the posterior predictive distribution. In
#'  this case, first, the draws from the posterior predictive distribution
#'  \emph{not conditioned} on group-level terms (\code{posterior_predict(..., re.form = NA)})
#'  are calculated as well as draws from this distribution \emph{conditioned}
#'  on \emph{all random effects} (by default, unless specified else in \code{re.form})
#'  are taken. Then, second, the variances for each of these draws are calculated.
#'  The "ICC" is then the ratio between these two variances. This is the recommended
#'  way to analyse random-effect-variances for non-Gaussian models. It is then possible
#'  to compare variances accross models, also by specifying different group-level
#'  terms via the \code{re.form}-argument.
#'  \cr \cr
#'  Sometimes, when the variance of the posterior predictive distribution is
#'  very large, the variance ratio in the output makes no sense, e.g. because
#'  it is negative. In such cases, it might help to use \code{robust = TRUE}.
#'
#' @examples
#' \dontrun{
#' library(lme4)
#' model <- lme4::lmer(Sepal.Length ~ Petal.Length + (1 | Species), data = iris)
#' icc(model)
#' }
#'
#' @importFrom insight model_info get_variance
#' @export
icc <- function(model, ...) {
  UseMethod("icc")
}


#' @export
icc.default <- function(model, ...) {
  if (!insight::model_info(model)$is_mixed) {
    stop("'model' has no random effects.", call. = FALSE)
  }

  vars <- insight::get_variance(model)

  # Calculate ICC values
  icc_adjusted <- vars$var.random / (vars$var.random + vars$var.residual)
  icc_conditional <- vars$var.random / (vars$var.fixed + vars$var.random + vars$var.residual)

  list(
    "ICC_adjusted" = icc_adjusted,
    "ICC_conditional" = icc_conditional
  )
}





#' @importFrom stats quantile
#' @rdname icc
#' @export
icc.brmsfit <- function(model, re.form = NULL, robust = TRUE, ci = .95, ...) {
  if (!insight::model_info(model)$is_mixed) {
    stop("'model' has no random effects.", call. = FALSE)
  }

  if (!requireNamespace("brms", quietly = TRUE)) {
    stop("Please install and load package `brms` first.", call. = FALSE)
  }

  PPD <- brms::posterior_predict(model, re.form = re.form, summary = FALSE)
  total_var <- apply(PPD, MARGIN = 1, FUN = stats::var)

  PPD_0 <- brms::posterior_predict(model, re.form = NA, summary = FALSE)
  tau.00 <- apply(PPD_0, MARGIN = 1, FUN = stats::var)

  if (robust)
    fun <- get("median", asNamespace("stats"))
  else
    fun <- get("mean", asNamespace("base"))

  list(
    "ICC_decomposed" = 1 - fun(tau.00 / total_var),
    "ICC_CI" = 1 - quantile(tau.00 / total_var, probs = c((1 - ci) / 2, (1 + ci) / 2))
  )
}