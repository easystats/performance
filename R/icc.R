#' The Intraclass Correlation Coefficient (ICC) for mixed models
#'
#' @description This function calculates the intraclass-correlation coefficient
#'  (icc) - sometimes also called \emph{variance partition coefficient}
#'  (vpc) - for mixed effects models. The ICC is calculated for \code{merMod}
#'  (\pkg{lme4}), \code{glmmTMB} (\pkg{glmmTMB}), \code{MixMod} (\pkg{GLMMadpative}),
#'  \code{lme} (\pkg{nlme}), \code{mixed} (\pkg{afex}), and \code{stanreg}
#'  (\pkg{rstanarm}) objects and can be interpreted as \dQuote{the proportion
#'  of the variance explained by the grouping structure in the population}
#'  \cite{(Hox 2010: 15)}. For models fitted with the \pkg{brms}-package,
#'  a variance decomposition based on the posterior predictive distribution
#'  is calculated (see 'Details').
#'
#' @param model A mixed effects model of class \code{merMod}, \code{glmmTMB},
#'  \code{MixMod}, \code{lme}, \code{mixed}, \code{stanreg} or \code{brmsfit}.
#' @param re.form Formula containing group-level effects to be considered in
#'   the prediction. If \code{NULL} (default), include all group-level effects.
#'   Else, for instance for nested models, name a specific group-level effect
#'   to calculate the variance decomposition for this group-level.
#' @param ci The Credible Interval level.
#' @param ... Currently not used.
#'
#' @inheritParams r2_bayes
#'
#' @return A list with two values, the adjusted and conditional ICC. For models
#'   of class \code{brmsfit}, a list with two values, the decomposed ICC as well
#'   as the credible intervals for this ICC.
#'
#' @references \itemize{
#'  \item Hox, J. J. (2010). Multilevel analysis: techniques and applications (2nd ed). New York: Routledge.
#'  \item Nakagawa, S., Johnson, P. C. D., & Schielzeth, H. (2017). The coefficient of determination R2 and intra-class correlation coefficient from generalized linear mixed-effects models revisited and expanded. Journal of The Royal Society Interface, 14(134), 20170213. \doi{10.1098/rsif.2017.0213}
#'  \item Rabe-Hesketh, S., & Skrondal, A. (2012). Multilevel and longitudinal modeling using Stata (3rd ed). College Station, Tex: Stata Press Publication.
#'  \item Raudenbush, S. W., & Bryk, A. S. (2002). Hierarchical linear models: applications and data analysis methods (2nd ed). Thousand Oaks: Sage Publications.
#'  }
#'
#' @details
#' \strong{Calculation of the ICC}
#'  \cr \cr
#'  The ICC is calculated by dividing the random effect variance,
#'  \ifelse{html}{\out{&sigma;<sup>2</sup><sub>i</sub>}}{\eqn{\sigma^2_i}}, by
#'  the total variance, i.e. the sum of the random effect variance and the
#'  residual variance, \ifelse{html}{\out{&sigma;<sup>2</sup><sub>&epsilon;</sub>}}{\eqn{\sigma^2_\epsilon}}.
#'  \cr \cr
#'  \strong{Adjusted and conditional ICC}
#'  \cr \cr
#'  \code{icc()} calculates an adjusted and conditional ICC, which both take
#'  all sources of uncertainty (i.e. of \emph{all random effects}) into account. While
#'  the \emph{adjusted ICC} only relates to the random effects, the \emph{conditional ICC}
#'  also takes the fixed effects variances into account (see \cite{Nakagawa et al. 2017}).
#'  Typically, the \emph{adjusted} ICC is of interest when the analysis of random
#'  effects is of interest. \code{icc()} returns a meaningful ICC also for more
#'  complex random effects structures, like models with random slopes or nested
#'  design (more than two levels) and is applicable for models with other distributions
#'  than Gaussian. For more details on the computation of the variances, see
#'  \code{\link[insight]{get_variance}}.
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
#'  \strong{ICC for specific group-levels }
#'  \cr \cr
#'  The proportion of variance for specific levels related to  each other
#'  (e.g., similarity of level-1-units within level-2-units or level-2-units
#'  within level-3-units) must be calculated manually. Use \code{\link[insight]{get_variance}}
#'  to get the random intercept variances (between-group-variances) and residual
#'  variance of the model, and calculate the ICC for the various level correlations.
#'  \cr \cr
#'  For example, for the ICC between level 1 and 2: \cr
#'  \code{sum(insight::get_variance_intercept(model)) /} \cr
#'  \code{  (sum(insight::get_variance_intercept(model)) + insight::get_variance_residual(model))}
#'  \cr \cr
#'  For for the ICC between level 2 and 3: \cr
#'  \code{insight::get_variance_intercept(model)[2] /} \cr
#'  \code{  sum(insight::get_variance_intercept(model))}
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
#' library(lme4)
#' model <- lme4::lmer(Sepal.Length ~ Petal.Length + (1 | Species), data = iris)
#' icc(model)
#'
#' @importFrom insight model_info get_variance print_color
#' @export
icc <- function(model, ...) {
  UseMethod("icc")
}


#' @export
icc.default <- function(model, ...) {
  if (!insight::model_info(model)$is_mixed) {
    stop("'model' has no random effects.", call. = FALSE)
  }

  vars <- tryCatch(
    {
      insight::get_variance(model, name_fun = "icc()", name_full = "ICC")
    },
    warning = function(e) {
      if (inherits(e, c("simpleWarning", "warning"))) {
        insight::print_color(e$message, "red")
        cat("\n")
      }
      NULL
    },
    error = function(e) {
      if (inherits(e, c("simpleError", "error"))) {
        insight::print_color(e$message, "red")
        cat("\n")
      }
      NULL
    }
  )


  if (is.null(vars) || all(is.na(vars))) {
    return(NA)
  }


  # Calculate ICC values
  icc_adjusted <- vars$var.random / (vars$var.random + vars$var.residual)
  icc_conditional <- vars$var.random / (vars$var.fixed + vars$var.random + vars$var.residual)

  structure(
    class = "icc",
    list(
      "ICC_adjusted" = icc_adjusted,
      "ICC_conditional" = icc_conditional
    )
  )
}



#' @importFrom bayestestR ci
#' @importFrom insight is_multivariate find_response model_info
#' @importFrom stats quantile var
#' @rdname icc
#' @export
icc.brmsfit <- function(model, re.form = NULL, robust = TRUE, ci = .95, ...) {

  ## TODO enable this once it is fixed in insight

  # for multivariate response models, we need a more complicated check...
  # if (insight::is_multivariate(model)) {
  #   resp <- insight::find_response(model)
  #
  #   is.mixed <- sapply(resp, function(i) {
  #     insight::model_info(model)[[resp]]$is_mixed
  #   }, simplify = TRUE)
  #
  #   if (!any(is.mixed)) {
  #     stop("'model' has no random effects.", call. = FALSE)
  #   }
  # } else if (!insight::model_info(model)$is_mixed) {
  #   stop("'model' has no random effects.", call. = FALSE)
  # }


  if (!insight::is_multivariate(model) && !insight::model_info(model)$is_mixed) {
    stop("'model' has no random effects.", call. = FALSE)
  }

  if (!requireNamespace("brms", quietly = TRUE)) {
    stop("Package `brms` needed for this function to work. Please install it.", call. = FALSE)
  }

  PPD <- brms::posterior_predict(model, re.form = re.form, summary = FALSE)
  var_total <- apply(PPD, MARGIN = 1, FUN = stats::var)

  PPD_0 <- brms::posterior_predict(model, re.form = NA, summary = FALSE)
  var_rand_intercept <- apply(PPD_0, MARGIN = 1, FUN = stats::var)

  if (robust)
    fun <- get("median", asNamespace("stats"))
  else
    fun <- get("mean", asNamespace("base"))

  var_icc <- var_rand_intercept / var_total
  var_residual <- var_total - var_rand_intercept
  ci_icc <- rev(1 - stats::quantile(var_rand_intercept / var_total, probs = c((1 - ci) / 2, (1 + ci) / 2)))

  result <- structure(
    class = "icc_decomposed",
    list(
      "ICC_decomposed" = 1 - fun(var_icc),
      "ICC_CI" = ci_icc
    )
  )

  attr(result, "var_rand_intercept") <- fun(var_rand_intercept)
  attr(result, "var_residual") <- fun(var_residual)
  attr(result, "var_total") <- fun(var_total)
  attr(result, "ci.var_rand_intercept") <- bayestestR::ci(var_rand_intercept, ci = ci)
  attr(result, "ci.var_residual") <- bayestestR::ci(var_residual, ci = ci)
  attr(result, "ci.var_total") <- bayestestR::ci(var_total, ci = ci)
  attr(result, "ci") <- ci
  attr(result, "re.form") <- re.form
  attr(result, "ranef") <- model$ranef$group[1]

  result
}