#' Intraclass Correlation Coefficient (ICC)
#'
#' This function calculates the intraclass-correlation coefficient (ICC) -
#' sometimes also called \emph{variance partition coefficient} (VPC) - for mixed
#' effects models. The ICC can be calculated for all models supported by
#' \code{insight::get_variance()}. For models fitted with the
#' \strong{brms}-package, \code{icc()} might fail due to the large variety of
#' models and families supported by the \strong{brms}-package. In such cases, an
#' alternative to the ICC is the \code{variance_decomposition()}, which is based
#' on the posterior predictive distribution (see 'Details').
#'
#' @param model A (Bayesian) mixed effects model.
#' @param re_formula Formula containing group-level effects to be considered in
#'   the prediction. If \code{NULL} (default), include all group-level effects.
#'   Else, for instance for nested models, name a specific group-level effect
#'   to calculate the variance decomposition for this group-level. See 'Details'
#'   and \code{?brms::posterior_predict}.
#' @param ci Credible interval level.
#' @param by_group Logical, if \code{TRUE}, \code{icc()} returns the variance
#'   components for each random-effects level (if there are multiple levels).
#'   See 'Details'.
#'
#' @inheritParams r2_bayes
#' @inheritParams insight::get_variance
#'
#' @return A list with two values, the adjusted and conditional ICC. For
#' \code{variance_decomposition()}, a list with two values, the decomposed
#' ICC as well as the credible intervals for this ICC.
#'
#' @references \itemize{
#'  \item Hox, J. J. (2010). Multilevel analysis: techniques and applications
#'  (2nd ed). New York: Routledge.
#'  \item Nakagawa, S., Johnson, P. C. D., & Schielzeth, H. (2017). The
#'  coefficient of determination R2 and intra-class correlation coefficient from
#'  generalized linear mixed-effects models revisited and expanded. Journal of
#'  The Royal Society Interface, 14(134), 20170213. \doi{10.1098/rsif.2017.0213}
#'  \item Rabe-Hesketh, S., & Skrondal, A. (2012). Multilevel and longitudinal
#'  modeling using Stata (3rd ed). College Station, Tex: Stata Press
#'  Publication.
#'  \item Raudenbush, S. W., & Bryk, A. S. (2002). Hierarchical linear models:
#'  applications and data analysis methods (2nd ed). Thousand Oaks: Sage
#'  Publications.
#'  }
#'
#' @details
#'  \subsection{Interpretation}{
#'  The ICC can be interpreted as \dQuote{the proportion of the variance
#'  explained by the grouping structure in the population}. The grouping
#'  structure entails that measurements are organized into groups (e.g., test
#'  scores in a school can be grouped by classroom if there are multiple
#'  classrooms and each classroom was administered the same test) and ICC indexes
#'  how strongly measurements in the same group resemble each other. This index
#'  goes from 0, if the grouping conveys no information, to 1, if all
#'  observations in a group are identical (Gelman \& Hill, 2007, p. 258). In
#'  other word, the ICC \dQuote{can also be interpreted as the expected
#'  correlation between two randomly drawn units that are in the same group}
#'  \cite{(Hox 2010: 15)}, although this definition might not apply to mixed
#'  models with more complex random effects structures.
#'  }
#'  \subsection{Calculation}{
#'  The ICC is calculated by dividing the random effect variance,
#'  \ifelse{html}{\out{&sigma;<sup>2</sup><sub>i</sub>}}{\eqn{\sigma^2_i}}, by
#'  the total variance, i.e. the sum of the random effect variance and the
#'  residual variance, \ifelse{html}{\out{&sigma;<sup>2</sup><sub>&epsilon;</sub>}}{\eqn{\sigma^2_\epsilon}}.
#'  }
#'  \subsection{Adjusted and conditional ICC}{
#'  \code{icc()} calculates an adjusted and conditional ICC, which both take all
#'  sources of uncertainty (i.e. of \emph{all random effects}) into account.
#'  While the \emph{adjusted ICC} only relates to the random effects, the
#'  \emph{conditional ICC} also takes the fixed effects variances into account
#'  (see \cite{Nakagawa et al. 2017}). Typically, the \emph{adjusted} ICC is of
#'  interest when the analysis of random effects is of interest. \code{icc()}
#'  returns a meaningful ICC also for more complex random effects structures,
#'  like models with random slopes or nested design (more than two levels) and
#'  is applicable for models with other distributions than Gaussian. For more
#'  details on the computation of the variances, see
#'  \code{?insight::get_variance}.
#'  }
#'  \subsection{ICC for unconditional and conditional models}{
#'  Usually, the ICC is calculated for the null model ("unconditional model").
#'  However, according to \cite{Raudenbush and Bryk (2002)} or
#'  \cite{Rabe-Hesketh and Skrondal (2012)} it is also feasible to compute the
#'  ICC for full models with covariates ("conditional models") and compare how
#'  much, e.g., a level-2 variable explains the portion of variation in the
#'  grouping structure (random intercept).
#'  }
#'  \subsection{ICC for specific group-levels}{
#'  The proportion of variance for specific levels related to the overall model
#'  can be computed by setting \code{by_group = TRUE}. The reported ICC is
#'  the variance for each (random effect) group compared to the total
#'  variance of the model. For mixed models with a simple random intercept,
#'  this is identical to the classical (adjusted) ICC.
#'  }
#'  \subsection{Variance decomposition for brms-models}{
#'  If \code{model} is of class \code{brmsfit}, \code{icc()} might fail due to
#'  the large variety of models and families supported by the \strong{brms}
#'  package. In such cases, \code{variance_decomposition()} is an alternative
#'  ICC measure. The function calculates a variance decomposition based on the
#'  posterior predictive distribution. In this case, first, the draws from the
#'  posterior predictive distribution \emph{not conditioned} on group-level
#'  terms (\code{posterior_predict(..., re_formula = NA)}) are calculated as
#'  well as draws from this distribution \emph{conditioned} on \emph{all random
#'  effects} (by default, unless specified else in \code{re_formula}) are taken.
#'  Then, second, the variances for each of these draws are calculated. The
#'  "ICC" is then the ratio between these two variances. This is the recommended
#'  way to analyse random-effect-variances for non-Gaussian models. It is then
#'  possible to compare variances across models, also by specifying different
#'  group-level terms via the \code{re_formula}-argument.
#'  \cr \cr
#'  Sometimes, when the variance of the posterior predictive distribution is
#'  very large, the variance ratio in the output makes no sense, e.g. because
#'  it is negative. In such cases, it might help to use \code{robust = TRUE}.
#'  }
#'
#' @examples
#' if (require("lme4")) {
#'   model <- lmer(Sepal.Length ~ Petal.Length + (1 | Species), data = iris)
#'   icc(model)
#' }
#'
#' # ICC for specific group-levels
#' if (require("lme4")) {
#'   data(sleepstudy)
#'   set.seed(12345)
#'   sleepstudy$grp <- sample(1:5, size = 180, replace = TRUE)
#'   sleepstudy$subgrp <- NA
#'   for (i in 1:5) {
#'     filter_group <- sleepstudy$grp == i
#'     sleepstudy$subgrp[filter_group] <-
#'       sample(1:30, size = sum(filter_group), replace = TRUE)
#'   }
#'   model <- lmer(
#'     Reaction ~ Days + (1 | grp / subgrp) + (1 | Subject),
#'     data = sleepstudy
#'   )
#'   icc(model, by_group = TRUE)
#' }
#' @export
icc <- function(model, by_group = FALSE, tolerance = 1e-05) {

  # special handling for smicd::semLme()
  if (inherits(model, "sem") && inherits(model, "lme")) {
    return(model$icc)
  }

  if (insight::is_multivariate(model)) {
    if (inherits(model, "brmsfit")) {
      return(variance_decomposition(model))
    } else {
      insight::print_color("Multiple response models not yet supported. You may use 'performance::variance_decomposition()'.\n", "red")
      return(NULL)
    }
  }

  if (!insight::is_mixed_model(model)) {
    warning("'model' has no random effects.", call. = FALSE)
    return(NULL)
  }

  vars <- tryCatch(
    {
      insight::get_variance(model,
        name_fun = "icc()",
        name_full = "ICC",
        tolerance = tolerance
      )
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


  # check if we have successfully computed all variance components...

  components <- c("var.fixed", "var.random", "var.residual")
  check_elements <- sapply(components, function(.i) !is.null(vars[[.i]]))

  if (!all(check_elements)) {
    return(NA)
  }


  # Calculate ICC values by groups
  if (isTRUE(by_group)) {
    # with random slopes, icc is inaccurate
    if (!is.null(insight::find_random_slopes(model))) {
      warning(insight::format_message("Model contains random slopes. Cannot compute accurate ICCs by group factors."), call. = FALSE)
    }

    # icc per group factor with reference to overall model
    icc_overall <- vars$var.intercept / (vars$var.random + vars$var.residual)

    out <- data.frame(
      Group = names(icc_overall),
      ICC = unname(icc_overall),
      stringsAsFactors = FALSE
    )

    # iccs between groups
    # n_grps <- length(vars$var.intercept)
    # level_combinations <- utils::combn(1:n_grps, m = n_grps - 1, simplify = FALSE)
    # icc_grp <- sapply(level_combinations, function(v) vars$var.intercept[v[1]] / (vars$var.intercept[v[1]] + vars$var.intercept[v[2]]))
    #
    # out2 <- data.frame(
    #   Group1 = group_names[sapply(level_combinations, function(i) i[1])],
    #   Group2 = group_names[sapply(level_combinations, function(i) i[2])],
    #   ICC = unname(icc_grp),
    #   stringsAsFactors = FALSE
    # )

    class(out) <- c("icc_by_group", class(out))
    out
  } else {
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
}



#' @export
as.data.frame.icc <- function(x, row.names = NULL, optional = FALSE, ...) {
  data.frame(ICC_adjusted = x$ICC_adjusted,
             ICC_conditional = x$ICC_conditional,
             stringsAsFactors = FALSE,
             row.names = row.names,
             optional = optional,
             ...)
}



#' @param ... Arguments passed down to \code{brms::posterior_predict()}.
#' @inheritParams icc
#' @rdname icc
#' @export
variance_decomposition <- function(model,
                                   re_formula = NULL,
                                   robust = TRUE,
                                   ci = .95,
                                   ...) {
  if (!inherits(model, "brmsfit")) {
    stop("Only models from package 'brms' are supported.")
  }

  mi <- insight::model_info(model)

  # for multivariate response models, we need a more complicated check...
  if (insight::is_multivariate(model)) {
    resp <- insight::find_response(model)
    is.mixed <- unlist(lapply(resp, function(i) mi[[i]]$is_mixed))
    if (!any(is.mixed)) {
      warning("'model' has no random effects.", call. = FALSE)
      return(NULL)
    }
  } else if (!insight::is_mixed_model(model)) {
    warning("'model' has no random effects.", call. = FALSE)
    return(NULL)
  }

  insight::check_if_installed("brms")

  PPD <- brms::posterior_predict(model, re_formula = re_formula, summary = FALSE, ...)
  var_total <- apply(PPD, MARGIN = 1, FUN = stats::var)

  PPD_0 <- brms::posterior_predict(model, re_formula = NA, summary = FALSE, ...)
  var_rand_intercept <- apply(PPD_0, MARGIN = 1, FUN = stats::var)

  if (robust) {
    fun <- get("median", asNamespace("stats"))
  } else {
    fun <- get("mean", asNamespace("base"))
  }

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
  attr(result, "re.form") <- re_formula
  attr(result, "ranef") <- model$ranef$group[1]

  # remove data
  attr(attr(result, "ci.var_rand_intercept"), "data") <- NULL
  attr(attr(result, "ci.var_residual"), "data") <- NULL
  attr(attr(result, "ci.var_total"), "data") <- NULL

  result
}
