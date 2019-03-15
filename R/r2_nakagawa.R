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
#' @importFrom insight get_variances
#' @export
r2_nakagawa <- function(model) {
  vars <- insight::get_variances(model)

  # Calculate R2 values

  r2_marginal <- vars$var.fixef / (vars$var.fixef + vars$var.ranef + vars$var.resid)
  r2_conditional <- (vars$var.fixef + vars$var.ranef) / (vars$var.fixef + vars$var.ranef + vars$var.resid)


  list(
    "R2_conditional" = r2_conditional,
    "R2_marginal" = r2_marginal
  )
}
