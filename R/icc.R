#' The Intraclass Correlation Coefficient (ICC) for mixed models
#'
#' @description This function calculates the intraclass-correlation
#'  (icc) - sometimes also called \emph{variance partition coefficient}
#'  (vpc) - for mixed effects models. Currently, \code{\link[lme4]{merMod}},
#'  \code{\link[glmmTMB]{glmmTMB}} and \code{stanreg} objects are supported.
#'  The ICC can be interpreted as \dQuote{the proportion of the variance
#'  explained by the grouping structure in the population} \cite{(Hox 2002: 15)}.
#'
#' @param model A mixed effects model of class \code{merMod}, \code{glmmTMB} or
#'  \code{stanreg}.
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
#'  \code{icc()} calculates an adjusted and conditional ICC are, which take
#'  all sources of uncertainty (of \emph{all random effects}) into account. While
#'  the adjusted ICC only relates to the random effects, the conditional ICC
#'  also takes the fixed effects variances into account (see \cite{Nakagawa et al. 2017}).
#'  \code{icc()} return a meaningful ICC also for models with random slopes and
#'  is applicable for models other distributions than Gaussian.
#'  \cr \cr
#'  \strong{ICC for unconditional and conditional models}
#'  \cr \cr
#'  Usually, the ICC is calculated for the null model ("unconditional model").
#'  However, according to \cite{Raudenbush and Bryk (2002)} or
#'  \cite{Rabe-Hesketh and Skrondal (2012)} it is also feasible to compute the ICC
#'  for full models with covariates ("conditional models") and compare how
#'  much a level-2 variable explains the portion of variation in the grouping
#'  structure (random intercept).
#'
#' @examples
#' \dontrun{
#' library(lme4)
#' model <- lme4::lmer(Sepal.Length ~ Petal.Length + (1 | Species), data = iris)
#' icc(model)
#' }
#'
#' @export
icc <- function(model) {
  vars <- .compute_variances(model, name = "icc")


  # Calculate ICC values

  icc_adjusted <- vars$var.ranef / (vars$var.ranef + vars$var.resid)
  icc_conditional <- vars$var.ranef / (vars$var.fixef + vars$var.ranef + vars$var.resid)


  list(
    "ICC_adjusted" = icc_adjusted,
    "ICC_conditional" = icc_conditional
  )
}
