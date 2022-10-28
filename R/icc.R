#' Intraclass Correlation Coefficient (ICC)
#'
#' This function calculates the intraclass-correlation coefficient (ICC) -
#' sometimes also called *variance partition coefficient* (VPC) or
#' *repeatability* - for mixed effects models. The ICC can be calculated for all
#' models supported by `insight::get_variance()`. For models fitted with the
#' **brms**-package, `icc()` might fail due to the large variety of
#' models and families supported by the **brms**-package. In such cases, an
#' alternative to the ICC is the `variance_decomposition()`, which is based
#' on the posterior predictive distribution (see 'Details').
#'
#' @param model A (Bayesian) mixed effects model.
#' @param re_formula Formula containing group-level effects to be considered in
#'   the prediction. If `NULL` (default), include all group-level effects.
#'   Else, for instance for nested models, name a specific group-level effect
#'   to calculate the variance decomposition for this group-level. See 'Details'
#'   and `?brms::posterior_predict`.
#' @param ci Confidence resp. credible interval level. For `icc()` and `r2()`,
#'   confidence intervals are based on bootstrapped samples from the ICC resp.
#'   R2 value. See `iterations`.
#' @param by_group Logical, if `TRUE`, `icc()` returns the variance
#'   components for each random-effects level (if there are multiple levels).
#'   See 'Details'.
#' @param iterations Number of bootstrap-replicates when computing confidence
#'   intervals for the ICC or R2.
#' @param ... Arguments passed down to `lme4::bootMer()` or `boot::boot()`
#'   for bootstrapped ICC or R2.
#'
#' @inheritParams r2_bayes
#' @inheritParams insight::get_variance
#'
#' @return A list with two values, the adjusted ICC and the unadjusted ICC. For
#' `variance_decomposition()`, a list with two values, the decomposed
#' ICC as well as the credible intervals for this ICC.
#'
#' @references
#'  - Hox, J. J. (2010). Multilevel analysis: techniques and applications
#'    (2nd ed). New York: Routledge.
#'  - Nakagawa, S., Johnson, P. C. D., and Schielzeth, H. (2017). The
#'    coefficient of determination R2 and intra-class correlation coefficient
#'    from generalized linear mixed-effects models revisited and expanded.
#'    Journal of The Royal Society Interface, 14(134), 20170213.
#'    \doi{10.1098/rsif.2017.0213}
#'  - Rabe-Hesketh, S., and Skrondal, A. (2012). Multilevel and longitudinal
#'    modeling using Stata (3rd ed). College Station, Tex: Stata Press
#'    Publication.
#'  - Raudenbush, S. W., and Bryk, A. S. (2002). Hierarchical linear models:
#'    applications and data analysis methods (2nd ed). Thousand Oaks: Sage
#'    Publications.
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
#'  observations in a group are identical (Gelman and Hill, 2007, p. 258). In
#'  other word, the ICC - sometimes conceptualized as the measurement
#'  repeatability - \dQuote{can also be interpreted as the expected
#'  correlation between two randomly drawn units that are in the same group}
#'  \cite{(Hox 2010: 15)}, although this definition might not apply to mixed
#'  models with more complex random effects structures. The ICC can help determine
#'  whether a mixed model is even necessary: an ICC of zero means the
#'  observations within clusters are no more similar than observations from
#'  different clusters, and setting it as a random factor might not be necessary.
#'  }
#'  \subsection{Difference with R2}{
#'  The coefficient of determination R2 (that can be computed with [`r2()`])
#'  quantifies the proportion of variance explained by a statistical model, but
#'  its definition in mixed model is complex (hence, different methods to compute
#'  a proxy exist). ICC is related to R2 because they are both ratios of
#'  variance components. More precisely, R2 is the proportion of the explained
#'  variance (of the full model), while the ICC is the proportion of explained
#'  variance that can be attributed to the random effects. In simple cases, the
#'  ICC corresponds to the difference between the *conditional R2* and the
#'  *marginal R2* (see [`r2_nakagawa()`]).
#'  }
#'  \subsection{Calculation}{
#'  The ICC is calculated by dividing the random effect variance,
#'  \ifelse{html}{\out{&sigma;<sup>2</sup><sub>i</sub>}}{\eqn{\sigma^2_i}}, by
#'  the total variance, i.e. the sum of the random effect variance and the
#'  residual variance, \ifelse{html}{\out{&sigma;<sup>2</sup><sub>&epsilon;</sub>}}{\eqn{\sigma^2_\epsilon}}.
#'  }
#'  \subsection{Adjusted and unadjusted ICC}{
#'  `icc()` calculates an adjusted and an unadjusted ICC, which both
#'  take all sources of uncertainty (i.e. of *all random effects*) into account.
#'  While the *adjusted ICC* only relates to the random effects, the
#'  *unadjusted ICC* also takes the fixed effects variances into account, more precisely,
#'  the fixed effects variance is added to the denominator of the formula to
#'  calculate the ICC (see \cite{Nakagawa et al. 2017}). Typically, the *adjusted* ICC is of
#'  interest when the analysis of random effects is of interest. `icc()`
#'  returns a meaningful ICC also for more complex random effects structures,
#'  like models with random slopes or nested design (more than two levels) and
#'  is applicable for models with other distributions than Gaussian. For more
#'  details on the computation of the variances, see
#'  `?insight::get_variance`.
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
#'  can be computed by setting `by_group = TRUE`. The reported ICC is
#'  the variance for each (random effect) group compared to the total
#'  variance of the model. For mixed models with a simple random intercept,
#'  this is identical to the classical (adjusted) ICC.
#'  }
#'  \subsection{Variance decomposition for brms-models}{
#'  If `model` is of class `brmsfit`, `icc()` might fail due to
#'  the large variety of models and families supported by the **brms**
#'  package. In such cases, `variance_decomposition()` is an alternative
#'  ICC measure. The function calculates a variance decomposition based on the
#'  posterior predictive distribution. In this case, first, the draws from the
#'  posterior predictive distribution *not conditioned* on group-level
#'  terms (`posterior_predict(..., re_formula = NA)`) are calculated as
#'  well as draws from this distribution *conditioned* on *all random
#'  effects* (by default, unless specified else in `re_formula`) are taken.
#'  Then, second, the variances for each of these draws are calculated. The
#'  "ICC" is then the ratio between these two variances. This is the recommended
#'  way to analyse random-effect-variances for non-Gaussian models. It is then
#'  possible to compare variances across models, also by specifying different
#'  group-level terms via the `re_formula`-argument.
#'  \cr \cr
#'  Sometimes, when the variance of the posterior predictive distribution is
#'  very large, the variance ratio in the output makes no sense, e.g. because
#'  it is negative. In such cases, it might help to use `robust = TRUE`.
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
icc <- function(model, by_group = FALSE, tolerance = 1e-05, ci = NULL, iterations = 100, ...) {
  # special handling for smicd::semLme()
  if (inherits(model, "sem") && inherits(model, "lme")) {
    return(model$icc)
  }

  if (insight::is_multivariate(model)) {
    if (inherits(model, "brmsfit")) {
      return(variance_decomposition(model))
    } else {
      insight::print_color(
        "Multiple response models not yet supported. You may use `performance::variance_decomposition()`.\n",
        "red"
      )
      return(NULL)
    }
  }

  if (!insight::is_mixed_model(model)) {
    warning("'model' has no random effects.", call. = FALSE)
    return(NULL)
  }

  # calculate random effect variances
  vars <- .compute_random_vars(model, tolerance)

  # return if ICC couldn't be computed
  if (is.null(vars) || all(is.na(vars))) {
    return(vars)
  }

  # Calculate ICC values by groups
  if (isTRUE(by_group)) {
    # with random slopes, icc is inaccurate
    if (!is.null(insight::find_random_slopes(model))) {
      insight::format_warning(
        "Model contains random slopes. Cannot compute accurate ICCs by group factors."
      )
    }

    if (!is.null(ci) && !is.na(ci)) {
      insight::format_warning("Confidence intervals are not yet supported for `by_group = TRUE`.")
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
  } else {
    # Calculate ICC values
    icc_adjusted <- vars$var.random / (vars$var.random + vars$var.residual)
    icc_unadjusted <- vars$var.random / (vars$var.fixed + vars$var.random + vars$var.residual)

    out <- data.frame(
      ICC_adjusted = icc_adjusted,
      ICC_conditional = icc_unadjusted,
      ICC_unadjusted = icc_unadjusted
    )

    # check if CIs are requested, and compute bootstrapped CIs
    if (!is.null(ci) && !is.na(ci)) {
      result <- .bootstrap_icc(model, iterations, tolerance, ...)
      out$CI <- ci
      # CI for adjusted ICC
      icc_ci <- as.vector(result$t[, 1])
      icc_ci <- icc_ci[!is.na(icc_ci)]
      icc_ci <- bayestestR::eti(icc_ci, ci = ci)
      out$CI_low_adjusted <- icc_ci$CI_low
      out$CI_high_adjusted <- icc_ci$CI_high

      # CI for unadjusted ICC
      icc_ci <- as.vector(result$t[, 2])
      icc_ci <- icc_ci[!is.na(icc_ci)]
      icc_ci <- bayestestR::eti(icc_ci, ci = ci)
      out$CI_low_unadjusted <- icc_ci$CI_low
      out$CI_high_unadjusted <- icc_ci$CI_high
    }

    class(out) <- c("icc", "data.frame")
  }
  out
}



#' @param ... Arguments passed down to `brms::posterior_predict()`.
#' @inheritParams icc
#' @rdname icc
#' @export
variance_decomposition <- function(model,
                                   re_formula = NULL,
                                   robust = TRUE,
                                   ci = .95,
                                   ...) {
  if (!inherits(model, "brmsfit")) {
    insight::format_error("Only models from package `brms` are supported.")
  }

  mi <- insight::model_info(model)

  # for multivariate response models, we need a more complicated check...
  if (insight::is_multivariate(model)) {
    resp <- insight::find_response(model)
    is.mixed <- unlist(lapply(resp, function(i) mi[[i]]$is_mixed))
    if (!any(is.mixed)) {
      warning("`model` has no random effects.", call. = FALSE)
      return(NULL)
    }
  } else if (!insight::is_mixed_model(model)) {
    warning("`model` has no random effects.", call. = FALSE)
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



# methods ------------------------------

#' @export
as.data.frame.icc <- function(x, row.names = NULL, optional = FALSE, ...) {
  data.frame(
    ICC_adjusted = x$ICC_adjusted,
    ICC_conditional = x$ICC_unadjusted,
    stringsAsFactors = FALSE,
    row.names = row.names,
    optional = optional,
    ...
  )
}


#' @export
print.icc <- function(x, digits = 3, ...) {
  insight::print_color("# Intraclass Correlation Coefficient\n\n", "blue")

  if (!is.null(x$CI_low_adjusted) && !is.null(x$CI_low_unadjusted)) {
    out <- paste0(
      c(
        sprintf(
          "    Adjusted ICC: %.*f %s",
          digits,
          x$ICC_adjusted,
          insight::format_ci(x$CI_low_adjusted, x$CI_high_adjusted, digits = digits, ci = NULL)
        ),
        sprintf(
          "  Unadjusted ICC: %.*f %s",
          digits,
          x$ICC_unadjusted,
          insight::format_ci(x$CI_low_unadjusted, x$CI_high_unadjusted, digits = digits, ci = NULL)
        )
      ),
      collapse = "\n"
    )
  } else {
    out <- paste0(
      c(
        sprintf("    Adjusted ICC: %.*f", digits, x$ICC_adjusted),
        sprintf("  Unadjusted ICC: %.*f", digits, x$ICC_unadjusted)
      ),
      collapse = "\n"
    )
  }


  cat(out)
  cat("\n")
  invisible(x)
}


#' @export
print.icc_by_group <- function(x, digits = 3, ...) {
  insight::print_color("# ICC by Group\n\n", "blue")
  cat(insight::export_table(x, digits = digits))
  invisible(x)
}


#' @export
print.icc_decomposed <- function(x, digits = 2, ...) {
  # print model information
  cat("# Random Effect Variances and ICC\n\n")

  reform <- attr(x, "re.form", exact = TRUE)
  if (is.null(reform)) {
    reform <- "all random effects"
  } else {
    reform <- insight::safe_deparse(reform)
  }

  cat(sprintf("Conditioned on: %s\n\n", reform))

  prob <- attr(x, "ci", exact = TRUE)

  cat(insight::print_color("## Variance Ratio (comparable to ICC)\n", "blue"))

  icc.val <- sprintf("%.*f", digits, x$ICC_decomposed)

  ci.icc.lo <- sprintf("%.*f", digits, x$ICC_CI[1])
  ci.icc.hi <- sprintf("%.*f", digits, x$ICC_CI[2])

  # ICC
  cat(sprintf(
    "Ratio: %s  CI %i%%: [%s %s]\n",
    icc.val,
    as.integer(round(prob * 100)),
    ci.icc.lo,
    ci.icc.hi
  ))

  cat(insight::print_color("\n## Variances of Posterior Predicted Distribution\n", "blue"))

  null.model <- sprintf("%.*f", digits, attr(x, "var_rand_intercept", exact = TRUE))

  ci.null <- attr(x, "ci.var_rand_intercept", exact = TRUE)
  ci.null.lo <- sprintf("%.*f", digits, ci.null$CI_low)
  ci.null.hi <- sprintf("%.*f", digits, ci.null$CI_high)

  full.model <- sprintf("%.*f", digits, attr(x, "var_total", exact = TRUE))

  ci.full <- attr(x, "ci.var_total", exact = TRUE)
  ci.full.lo <- sprintf("%.*f", digits, ci.full$CI_low)
  ci.full.hi <- sprintf("%.*f", digits, ci.full$CI_high)

  ml <- max(nchar(null.model), nchar(full.model))
  ml.ci <- max(nchar(ci.full.lo), nchar(ci.null.lo))
  mh.ci <- max(nchar(ci.full.hi), nchar(ci.null.hi))

  # Conditioned on fixed effects
  cat(sprintf(
    "Conditioned on fixed effects: %*s  CI %i%%: [%*s %*s]\n",
    ml,
    null.model,
    as.integer(round(prob * 100)),
    ml.ci,
    ci.null.lo,
    mh.ci,
    ci.null.hi
  ))

  # Conditioned on random effects
  cat(sprintf(
    "Conditioned on rand. effects: %*s  CI %i%%: [%*s %*s]\n",
    ml,
    full.model,
    as.integer(round(prob * 100)),
    ml.ci,
    ci.full.lo,
    mh.ci,
    ci.full.hi
  ))

  cat(insight::print_color("\n## Difference in Variances\n", "red"))

  res <- sprintf("%.*f", digits, attr(x, "var_residual", exact = TRUE))

  ci.res <- attr(x, "ci.var_residual", exact = TRUE)
  ci.res.lo <- sprintf("%.*f", digits, ci.res$CI_low)
  ci.res.hi <- sprintf("%.*f", digits, ci.res$CI_high)

  # ICC
  cat(sprintf(
    "Difference: %s  CI %i%%: [%s %s]\n",
    res,
    as.integer(round(prob * 100)),
    ci.res.lo,
    ci.res.hi
  ))

  invisible(x)
}



# helper -----------------

.compute_random_vars <- function(model,
                                 tolerance,
                                 components = c("var.fixed", "var.random", "var.residual"),
                                 name_fun = "icc()",
                                 name_full = "ICC",
                                 verbose = TRUE) {
  vars <- tryCatch(
    {
      insight::get_variance(model,
        name_fun = name_fun,
        name_full = name_full,
        tolerance = tolerance,
        verbose = verbose
      )
    },
    error = function(e) {
      if (inherits(e, c("simpleError", "error")) && verbose) {
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
  check_elements <- sapply(components, function(.i) !is.null(vars[[.i]]))

  if (!all(check_elements)) {
    return(NA)
  }

  vars
}


# bootstrapping ------------------

# bootstrapping using package "boot"
.boot_icc_fun <- function(data, indices, model, tolerance) {
  d <- data[indices, ] # allows boot to select sample
  fit <- suppressWarnings(suppressMessages(stats::update(model, data = d)))
  vars <- .compute_random_vars(fit, tolerance, verbose = FALSE)
  if (is.null(vars) || all(is.na(vars))) {
    return(c(NA, NA))
  }
  c(
    vars$var.random / (vars$var.random + vars$var.residual),
    vars$var.random / (vars$var.fixed + vars$var.random + vars$var.residual)
  )
}

# bootstrapping using "lme4::bootMer"
.boot_icc_fun_lme4 <- function(model) {
  vars <- .compute_random_vars(model, tolerance = 1e-05, verbose = FALSE)
  if (is.null(vars) || all(is.na(vars))) {
    return(c(NA, NA))
  }
  c(
    vars$var.random / (vars$var.random + vars$var.residual),
    vars$var.random / (vars$var.fixed + vars$var.random + vars$var.residual)
  )
}

# prepare arguments for "lme4::bootMer"
.do_lme4_bootmer <- function(model, .boot_fun, iterations, dots) {
  insight::check_if_installed(c("lme4", "boot"))
  args <- list(
    model,
    .boot_fun,
    nsim = iterations,
    type = "parametric",
    parallel = "no",
    use.u = FALSE,
    ncpus = 1
  )
  # add/overwrite dot-args
  if (!is.null(dots[["use.u"]])) {
    args$use.u <- dots[["use.u"]]
  }
  if (!is.null(dots[["re.form"]])) {
    args$re.form <- dots[["re.form"]]
  }
  if (!is.null(dots[["type"]])) {
    args$type <- dots[["type"]]
    if (args$type == "semiparametric") {
      args$use.u <- TRUE
    }
  }
  if (!is.null(dots[["parallel"]])) {
    args$parallel <- dots[["parallel"]]
  }
  if (!is.null(dots[["ncpus"]])) {
    args$ncpus <- dots[["ncpus"]]
  }
  # bootsrap
  do.call(lme4::bootMer, args)
}

# main function for bootstrapping
.bootstrap_icc <- function(model, iterations, tolerance, ...) {
  if (inherits(model, c("merMod", "lmerMod", "glmmTMB"))) {
    result <- .do_lme4_bootmer(
      model,
      .boot_icc_fun_lme4,
      iterations,
      dots = list(...)
    )
  } else {
    insight::check_if_installed("boot")
    result <- boot::boot(
      data = insight::get_data(model),
      statistic = .boot_icc_fun,
      R = iterations,
      sim = "ordinary",
      model = model,
      tolerance = tolerance
    )
  }
  result
}
