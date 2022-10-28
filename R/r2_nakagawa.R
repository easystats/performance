#' @title Nakagawa's R2 for mixed models
#' @name r2_nakagawa
#'
#' @description Compute the _marginal_ and _conditional_ r-squared value for
#'  mixed effects models with complex random effects structures.
#'
#' @param model A mixed effects model.
#' @param by_group Logical, if `TRUE`, returns the explained variance
#'   at different levels (if there are multiple levels). This is essentially
#'   similar to the variance reduction approach by _Hox (2010), pp. 69-78_.
#' @param tolerance Tolerance for singularity check of random effects, to decide
#'   whether to compute random effect variances for the conditional r-squared
#'   or not. Indicates up to which value the convergence result is accepted. When
#'   `r2_nakagawa()` returns a warning, stating that random effect variances
#'   can't be computed (and thus, the conditional r-squared is `NA`),
#'   decrease the tolerance-level. See also [`check_singularity()`].
#' @inheritParams icc
#'
#' @return A list with the conditional and marginal R2 values.
#'
#' @details
#' Marginal and conditional r-squared values for mixed models are calculated
#' based on _Nakagawa et al. (2017)_. For more details on the computation of
#' the variances, see `?insight::get_variance`. The random effect variances are
#' actually the mean random effect variances, thus the r-squared value is also
#' appropriate for mixed models with random slopes or nested random effects
#' (see _Johnson, 2014_).
#'
#' - **Conditional R2**: takes both the fixed and random effects into account.
#' - **Marginal R2**: considers only the variance of the fixed effects.
#'
#' The contribution of random effects can be deduced by subtracting the
#' marginal R2 from the conditional R2 or by computing the [`icc()`].
#'
#' @references
#'  - Hox, J. J. (2010). Multilevel analysis: techniques and applications
#'    (2nd ed). New York: Routledge.
#'  - Johnson, P. C. D. (2014). Extension of Nakagawa and Schielzeth’s R2 GLMM
#'    to random slopes models. Methods in Ecology and Evolution, 5(9), 944–946.
#'    \doi{10.1111/2041-210X.12225}
#'  - Nakagawa, S., and Schielzeth, H. (2013). A general and simple method for
#'    obtaining R2 from generalized linear mixed-effects models. Methods in
#'    Ecology and Evolution, 4(2), 133–142. \doi{10.1111/j.2041-210x.2012.00261.x}
#'  - Nakagawa, S., Johnson, P. C. D., and Schielzeth, H. (2017). The
#'    coefficient of determination R2 and intra-class correlation coefficient from
#'    generalized linear mixed-effects models revisited and expanded. Journal of
#'    The Royal Society Interface, 14(134), 20170213. \doi{10.1098/rsif.2017.0213}
#'
#' @examples
#' if (require("lme4")) {
#'   model <- lmer(Sepal.Length ~ Petal.Length + (1 | Species), data = iris)
#'   r2_nakagawa(model)
#'   r2_nakagawa(model, by_group = TRUE)
#' }
#' @export
r2_nakagawa <- function(model, by_group = FALSE, tolerance = 1e-5, ci = NULL, iterations = 100, ...) {
  # calculate random effect variances
  vars <- .compute_random_vars(
    model,
    tolerance,
    components = c("var.fixed", "var.residual"),
    name_fun = "r2()",
    name_full = "r-squared"
  )

  # return if R2 couldn't be computed
  if (is.null(vars) || all(is.na(vars))) {
    return(vars)
  }

  # compute R2 by group
  if (isTRUE(by_group)) {
    # with random slopes, explained variance is inaccurate
    if (!is.null(insight::find_random_slopes(model))) {
      insight::format_warning("Model contains random slopes. Explained variance by levels is not accurate.")
    }

    if (!is.null(ci) && !is.na(ci)) {
      insight::format_warning("Confidence intervals are not yet supported for `by_group = TRUE`.")
    }

    # null-model
    null_model <- insight::null_model(model)
    vars_null <- insight::get_variance(null_model, tolerance = tolerance)

    # names of group levels
    group_names <- insight::find_random(model, split_nested = TRUE, flatten = TRUE)

    # compute r2 by level
    r2_random <- 1 - (vars$var.intercept[group_names] / vars_null$var.intercept[group_names])
    r2_fixed <- 1 - (vars$var.residual / vars_null$var.residual)

    out <- data.frame(
      Level = c("Level 1", group_names),
      R2 = c(r2_fixed, r2_random),
      stringsAsFactors = FALSE
    )

    class(out) <- c("r2_nakagawa_by_group", "data.frame")
  } else {
    # Calculate R2 values
    if (insight::is_empty_object(vars$var.random) || is.na(vars$var.random)) {
      # if no random effect variance, return simple R2
      insight::print_color("Random effect variances not available. Returned R2 does not account for random effects.\n", "red")
      r2_marginal <- vars$var.fixed / (vars$var.fixed + vars$var.residual)
      r2_conditional <- NA
    } else {
      r2_marginal <- vars$var.fixed / (vars$var.fixed + vars$var.random + vars$var.residual)
      r2_conditional <- (vars$var.fixed + vars$var.random) / (vars$var.fixed + vars$var.random + vars$var.residual)
    }

    names(r2_conditional) <- "Conditional R2"
    names(r2_marginal) <- "Marginal R2"
    out <- list(R2_conditional = r2_conditional, R2_marginal = r2_marginal)

    # check if CIs are requested, and compute bootstrapped CIs
    if (!is.null(ci) && !is.na(ci)) {
      result <- .bootstrap_r2_nakagawa(model, iterations, tolerance, ...)
      out$CI <- ci
      # CI for marginal R2
      icc_ci <- as.vector(result$t[, 1])
      icc_ci <- icc_ci[!is.na(icc_ci)]
      icc_ci <- bayestestR::eti(icc_ci, ci = ci)
      out$CI_low_marginal <- icc_ci$CI_low
      out$CI_high_marginal <- icc_ci$CI_high

      # CI for unadjusted R2
      icc_ci <- as.vector(result$t[, 2])
      icc_ci <- icc_ci[!is.na(icc_ci)]
      icc_ci <- bayestestR::eti(icc_ci, ci = ci)
      out$CI_low_conditional <- icc_ci$CI_low
      out$CI_high_conditional <- icc_ci$CI_high
    }

    class(out)  <- c("r2_nakagawa", "list")
  }
  out
}



# methods ------

#' @export
as.data.frame.r2_nakagawa <- function(x, row.names = NULL, optional = FALSE, ...) {
  data.frame(
    R2_conditional = x$R2_conditional,
    R2_marginal = x$R2_marginal,
    stringsAsFactors = FALSE,
    row.names = row.names,
    optional = optional,
    ...
  )
}


#' @export
print.r2_nakagawa <- function(x, digits = 3, ...) {
  model_type <- attr(x, "model_type")
  if (is.null(model_type)) {
    insight::print_color("# R2 for Mixed Models\n\n", "blue")
  } else {
    insight::print_color("# R2 for %s Regression\n\n", "blue")
  }

  if (!is.null(x$CI_low_marginal) && !is.null(x$CI_low_conditional)) {
    out <- paste0(
      c(
        sprintf(
          "  Conditional R2: %.*f %s",
          digits,
          x$R2_conditional,
          insight::format_ci(x$CI_low_conditional, x$CI_high_conditional, digits = digits, ci = NULL)
        ),
        sprintf(
          "     Marginal R2: %.*f %s",
          digits,
          x$R2_marginal,
          insight::format_ci(x$CI_low_marginal, x$CI_high_marginal, digits = digits, ci = NULL)
        )
      ),
      collapse = "\n"
    )
  } else {
    out <- paste0(
      c(
        sprintf("  Conditional R2: %.*f", digits, x$R2_conditional),
        sprintf("     Marginal R2: %.*f", digits, x$R2_marginal)
      ),
      collapse = "\n"
    )
  }

  cat(out)
  cat("\n")
  invisible(x)
}



# bootstrapping --------------

.boot_r2_fun <- function(data, indices, model, tolerance) {
  d <- data[indices, ] # allows boot to select sample
  fit <- suppressWarnings(suppressMessages(stats::update(model, data = d)))
  vars <- .compute_random_vars(fit, tolerance, verbose = FALSE)
  if (is.null(vars) || all(is.na(vars))) {
    return(c(NA, NA))
  }
  # Calculate R2 values
  if (insight::is_empty_object(vars$var.random) || is.na(vars$var.random)) {
    c(vars$var.fixed / (vars$var.fixed + vars$var.residual), NA)
  } else {
    c(
      vars$var.fixed / (vars$var.fixed + vars$var.random + vars$var.residual),
      (vars$var.fixed + vars$var.random) / (vars$var.fixed + vars$var.random + vars$var.residual)
    )
  }
}

.boot_r2_fun_lme4 <- function(model) {
  vars <- .compute_random_vars(model, tolerance = 1e-05, verbose = FALSE)
  if (is.null(vars) || all(is.na(vars))) {
    return(c(NA, NA))
  }
  # Calculate R2 values
  if (insight::is_empty_object(vars$var.random) || is.na(vars$var.random)) {
    c(vars$var.fixed / (vars$var.fixed + vars$var.residual), NA)
  } else {
    c(
      vars$var.fixed / (vars$var.fixed + vars$var.random + vars$var.residual),
      (vars$var.fixed + vars$var.random) / (vars$var.fixed + vars$var.random + vars$var.residual)
    )
  }
}

.bootstrap_r2_nakagawa <- function(model, iterations, tolerance, ...) {
  if (inherits(model, c("merMod", "lmerMod", "glmmTMB"))) {
    insight::check_if_installed(c("lme4", "boot"))
    dots <- list(...)
    args <- list(
      model,
      .boot_r2_fun_lme4,
      nsim = iterations,
      type = "parametric",
      parallel = "no",
      use.u = FALSE,
      ncpus = 1
    )

    # add dot-args
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
    result <- do.call(lme4::bootMer, args)

  } else {
    insight::check_if_installed("boot")
    result <- boot::boot(
      data = insight::get_data(model),
      statistic = .boot_r2_fun,
      R = iterations,
      sim = "ordinary",
      model = model,
      tolerance = tolerance
    )
  }
  result
}
