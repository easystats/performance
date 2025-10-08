#' @title Check overdispersion (and underdispersion) of GL(M)M's
#' @name check_overdispersion
#'
#' @description `check_overdispersion()` checks generalized linear (mixed)
#'   models for overdispersion (and underdispersion).
#'
#' @param x Fitted model of class `merMod`, `glmmTMB`, `glm`, or `glm.nb`
#'   (package **MASS**), or an object returned by `simulate_residuals()`.
#'
#' @inheritParams check_zeroinflation
#'
#' @return A list with results from the overdispersion test, like chi-squared
#'   statistics, p-value or dispersion ratio.
#'
#' @details Overdispersion occurs when the observed variance is higher than the
#' variance of a theoretical model. For Poisson models, variance increases
#' with the mean and, therefore, variance usually (roughly) equals the mean
#' value. If the variance is much higher, the data are "overdispersed". A less
#' common case is underdispersion, where the variance is much lower than the
#' mean.
#'
#' @section Interpretation of the Dispersion Ratio:
#' If the dispersion ratio is close to one, a Poisson model fits well to the
#' data. Dispersion ratios larger than one indicate overdispersion, thus a
#' negative binomial model or similar might fit better to the data. Dispersion
#' ratios much smaller than one indicate underdispersion. A p-value < .05
#' indicates either overdispersion or underdispersion (the first being more common).
#'
#' @section Overdispersion in Poisson Models:
#' For Poisson models, the overdispersion test is based on the code from
#' _Gelman and Hill (2007), page 115_.
#'
#' @section Overdispersion in Negative Binomial or Zero-Inflated Models:
#' For negative binomial (mixed) models or models with zero-inflation component,
#' the overdispersion test is based simulated residuals (see [`simulate_residuals()`]).
#'
#' @section Overdispersion in Mixed Models:
#' For `merMod`- and `glmmTMB`-objects, `check_overdispersion()`
#' is based on the code in the
#' [GLMM FAQ](http://bbolker.github.io/mixedmodels-misc/glmmFAQ.html),
#' section *How can I deal with overdispersion in GLMMs?*. Note that this
#' function only returns an *approximate* estimate of an overdispersion
#' parameter. For Poisson, zero-inflated, or negative binomial mixed models
#' (fitted with `glmmTMB` or `lme4`), the overdispersion test is based on
#' [`simulate_residuals()`] (which is identical to
#' `check_overdispersion(simulate_residuals(model))`).
#'
#' @inheritSection check_zeroinflation Tests based on simulated residuals
#'
#' @section How to fix Overdispersion:
#' Overdispersion can be fixed by either modeling the dispersion parameter, or
#' by choosing a different distributional family (like Quasi-Poisson, or
#' negative binomial, see _Gelman and Hill (2007), pages 115-116_).
#'
#' @family functions to check model assumptions and and assess model quality
#'
#' @references
#' - Bolker B et al. (2017):
#'  [GLMM FAQ.](http://bbolker.github.io/mixedmodels-misc/glmmFAQ.html)
#'
#' - Gelman, A., and Hill, J. (2007). Data analysis using regression and
#'  multilevel/hierarchical models. Cambridge; New York: Cambridge University
#'  Press.
#'
#' @examplesIf getRversion() >= "4.0.0" && require("glmmTMB")
#' data(Salamanders, package = "glmmTMB")
#' m <- glm(count ~ spp + mined, family = poisson, data = Salamanders)
#' check_overdispersion(m)
#' @export
check_overdispersion <- function(x, ...) {
  UseMethod("check_overdispersion")
}


# default -----------------------

#' @export
check_overdispersion.default <- function(x, ...) {
  .is_model_valid(x)
  insight::format_error(
    paste0(
      "`check_overdisperion()` not yet implemented for models of class `",
      class(x)[1],
      "`."
    )
  )
}


# Methods -----------------------------

#' @export
plot.check_overdisp <- function(x, ...) {
  insight::check_if_installed(c("see", "graphics"), "for overdispersion plots")
  obj_name <- attr(x, "object_name", exact = TRUE)
  model <- NULL

  if (!is.null(obj_name)) {
    model <- .safe(get(obj_name, envir = parent.frame()))
    if (is.null(model)) {
      # second try, global env
      model <- .safe(get(obj_name, envir = globalenv()))
    }
  }
  if (!is.null(model)) {
    # TODO: For models that use simulated residuals in check_overdispersion()
    # (e.g., Poisson mixed models, zero-inflated models), this still uses
    # classical residuals for plotting. Consider using simulated residuals
    # for consistency. See #595, #643, #654
    x <- .model_diagnostic_overdispersion(model)
    class(x) <- c("see_check_overdisp", "data.frame")
    attr(x, "colors") <- list(...)$colors
    attr(x, "line_size") <- list(...)$size_line
    attr(x, "overdisp_type") <- list(...)$plot_type
    graphics::plot(x, ...)
  }
}

#' @export
print.check_overdisp <- function(x, digits = 3, ...) {
  orig_x <- x

  x$dispersion_ratio <- sprintf("%.*f", digits, x$dispersion_ratio)
  if (!is.null(x$chisq_statistic)) {
    x$chisq_statistic <- sprintf("%.*f", digits, x$chisq_statistic)
  }

  x$p_value <- pval <- round(x$p_value, digits = digits)
  if (x$p_value < 0.001) {
    x$p_value <- "< 0.001"
  }

  maxlen <- max(
    nchar(x$dispersion_ratio),
    nchar(x$chisq_statistic),
    nchar(x$p_value)
  )

  insight::print_color("# Overdispersion test\n\n", "blue")
  if (is.null(x$chisq_statistic)) {
    cat(sprintf(
      " dispersion ratio = %s\n",
      format(x$dispersion_ratio, justify = "right", width = maxlen)
    ))
    cat(sprintf(
      "          p-value = %s\n\n",
      format(x$p_value, justify = "right", width = maxlen)
    ))
  } else {
    cat(sprintf(
      "       dispersion ratio = %s\n",
      format(x$dispersion_ratio, justify = "right", width = maxlen)
    ))
    cat(sprintf(
      "  Pearson's Chi-Squared = %s\n",
      format(x$chisq_statistic, justify = "right", width = maxlen)
    ))
    cat(sprintf(
      "                p-value = %s\n\n",
      format(x$p_value, justify = "right", width = maxlen)
    ))
  }

  if (pval > 0.05) {
    message("No overdispersion detected.")
  } else if (x$dispersion_ratio > 1) {
    message("Overdispersion detected.")
  } else {
    message("Underdispersion detected.")
  }

  invisible(orig_x)
}


# Overdispersion for classical models -----------------------------

#' @export
check_overdispersion.glm <- function(x, verbose = TRUE, ...) {
  # model info
  info <- insight::model_info(x)
  obj_name <- insight::safe_deparse_symbol(substitute(x))

  # for certain distributions, simulated residuals are more accurate
  use_simulated <- info$is_bernoulli ||
    info$is_binomial ||
    (!info$is_count && !info$is_binomial) ||
    info$is_negbin

  # model classes not supported in DHARMa
  not_supported <- c("fixest", "glmx")

  if (use_simulated && !inherits(x, not_supported)) {
    return(check_overdispersion(simulate_residuals(x, ...), object_name = obj_name, ...))
  }

  # check if we have poisson - need this for models not supported by DHARMa
  if (!info$is_count && !info$is_binomial) {
    insight::format_error(
      "Overdispersion checks can only be used for models from Poisson families or binomial families with trials > 1."
    )
  }

  # check for Bernoulli
  if (info$is_bernoulli) {
    insight::format_error("Overdispersion checks cannot be used for Bernoulli models.")
  }

  yhat <- stats::fitted(x)

  n <- stats::nobs(x)
  k <- length(insight::find_parameters(x, effects = "fixed", flatten = TRUE))

  zi <- (insight::get_response(x, verbose = FALSE) - yhat) / sqrt(yhat)
  chisq <- sum(zi^2)
  ratio <- chisq / (n - k)
  p.value <- stats::pchisq(chisq, df = n - k, lower.tail = FALSE)

  out <- list(
    chisq_statistic = chisq,
    dispersion_ratio = ratio,
    residual_df = n - k,
    p_value = p.value
  )

  class(out) <- c("check_overdisp", "see_check_overdisp")
  attr(out, "object_name") <- obj_name

  out
}

#' @export
check_overdispersion.fixest <- check_overdispersion.glm

#' @export
check_overdispersion.fixest_multi <- function(x, verbose = TRUE, ...) {
  lapply(x, check_overdispersion.fixest)
}

#' @export
check_overdispersion.glmx <- check_overdispersion.glm


# mfx models ------------------------------

#' @export
check_overdispersion.poissonmfx <- function(x, ...) {
  check_overdispersion(x$fit, ...)
}

#' @export
check_overdispersion.poissonirr <- check_overdispersion.poissonmfx

#' @export
check_overdispersion.negbinirr <- check_overdispersion.poissonmfx

#' @export
check_overdispersion.negbinmfx <- check_overdispersion.poissonmfx

#' @export
check_overdispersion.model_fit <- check_overdispersion.poissonmfx


# Overdispersion for mixed models ---------------------------

#' @export
check_overdispersion.merMod <- function(x, ...) {
  # for certain distributions, simulated residuals are more accurate
  info <- insight::model_info(x)
  obj_name <- insight::safe_deparse_symbol(substitute(x))

  # for certain distributions, simulated residuals are more accurate
  # Note: now including Poisson models for mixed models (see #595, #643)
  use_simulated <- info$family == "genpois" ||
    info$is_zero_inflated ||
    info$is_bernoulli ||
    info$is_binomial ||
    (!info$is_count && !info$is_binomial) ||
    info$is_negbin ||
    info$is_poisson

  if (use_simulated) {
    return(check_overdispersion(simulate_residuals(x, ...), object_name = obj_name, ...))
  }

  rdf <- stats::df.residual(x)
  rp <- insight::get_residuals(x, type = "pearson")

  # check if pearson residuals are available
  if (insight::is_empty_object(rp)) {
    return(check_overdispersion(simulate_residuals(x, ...), object_name = obj_name, ...))
  }

  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq / rdf
  pval <- stats::pchisq(Pearson.chisq, df = rdf, lower.tail = FALSE)

  out <- list(
    chisq_statistic = Pearson.chisq,
    dispersion_ratio = prat,
    residual_df = rdf,
    p_value = pval
  )

  class(out) <- c("check_overdisp", "see_check_overdisp")
  attr(out, "object_name") <- obj_name

  out
}

#' @export
check_overdispersion.negbin <- check_overdispersion.merMod

#' @export
check_overdispersion.glmmTMB <- check_overdispersion.merMod


# simulated residuals -----------------------------

#' @rdname check_overdispersion
#' @export
check_overdispersion.performance_simres <- function(x, alternative = "two.sided", ...) {
  alternative <- insight::validate_argument(
    alternative,
    c("two.sided", "less", "greater")
  )

  # check for special arguments - we may pass "object_name" from other methods
  dots <- list(...)
  if (is.null(dots$object_name)) {
    obj_name <- insight::safe_deparse_symbol(substitute(x))
  } else {
    obj_name <- dots$object_name
  }

  # statistics function
  variance <- stats::sd(x$simulatedResponse)^2
  dispersion <- function(i) stats::var(i - x$fittedPredictedResponse) / variance

  # compute test results
  result <- .simres_statistics(x, statistic_fun = dispersion, alternative = alternative)

  out <- list(
    dispersion_ratio = result$observed / mean(result$simulated),
    p_value = result$p
  )

  class(out) <- c("check_overdisp", "see_check_overdisp")
  attr(out, "object_name") <- obj_name

  out
}

#' @export
check_overdispersion.DHARMa <- check_overdispersion.performance_simres
