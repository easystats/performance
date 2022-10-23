#' @title Check overdispersion of GL(M)M's
#' @name check_overdispersion
#'
#' @description `check_overdispersion()` checks generalized linear (mixed)
#'   models for overdispersion.
#'
#' @param x Fitted model of class `merMod`, `glmmTMB`, `glm`,
#'    or `glm.nb` (package \pkg{MASS}).
#' @param ... Currently not used.
#'
#' @return A list with results from the overdispersion test, like chi-squared
#'   statistics, p-value or dispersion ratio.
#'
#' @details Overdispersion occurs when the observed variance is higher than the
#'   variance of a theoretical model. For Poisson models, variance increases
#'   with the mean and, therefore, variance usually (roughly) equals the mean
#'   value. If the variance is much higher, the data are "overdispersed".
#'
#' \subsection{Interpretation of the Dispersion Ratio}{
#' If the dispersion ratio is close to one, a Poisson model fits well to the
#' data. Dispersion ratios larger than one indicate overdispersion, thus a
#' negative binomial model or similar might fit better to the data. A p-value <
#' .05 indicates overdispersion.
#' }
#'
#' \subsection{Overdispersion in Poisson Models}{
#' For Poisson models, the overdispersion test is based on the code from
#' \cite{Gelman and Hill (2007), page 115}.
#' }
#'
#' \subsection{Overdispersion in Mixed Models}{
#' For `merMod`- and `glmmTMB`-objects, `check_overdispersion()`
#' is based on the code in the
#' [GLMM FAQ](http://bbolker.github.io/mixedmodels-misc/glmmFAQ.html),
#' section *How can I deal with overdispersion in GLMMs?*. Note that this
#' function only returns an *approximate* estimate of an overdispersion
#' parameter, and is probably inaccurate for zero-inflated mixed models (fitted
#' with `glmmTMB`).
#' }
#'
#' \subsection{How to fix Overdispersion}{
#' Overdispersion can be fixed by either modeling the dispersion parameter, or
#' by choosing a different distributional family (like Quasi-Poisson, or
#' negative binomial, see \cite{Gelman and Hill (2007), pages 115-116}).
#' }
#'
#' @references \itemize{
#'  \item Bolker B et al. (2017):
#'  [GLMM FAQ.](http://bbolker.github.io/mixedmodels-misc/glmmFAQ.html)
#'  \item Gelman, A., and Hill, J. (2007). Data analysis using regression and
#'  multilevel/hierarchical models. Cambridge; New York: Cambridge University
#'  Press.
#'  }
#'
#'
#' @examplesIf getRversion() >= "4.0.0"
#' if (require("glmmTMB")) {
#'   data(Salamanders)
#'   m <- glm(count ~ spp + mined, family = poisson, data = Salamanders)
#'   check_overdispersion(m)
#'
#'   m <- glmmTMB(
#'     count ~ mined + spp + (1 | site),
#'     family = poisson,
#'     data = Salamanders
#'   )
#'   check_overdispersion(m)
#' }
#' @export
check_overdispersion <- function(x, ...) {
  UseMethod("check_overdispersion")
}



# default -----------------------

#' @export
check_overdispersion.default <- function(x, ...) {
  # check for valid input
  .is_model_valid(x)
  insight::format_error(
    paste0("`check_overdisperion()` not yet implemented for models of class `", class(x)[1], "`.")
  )
}



# Methods -----------------------------

#' @export
plot.check_overdisp <- function(x, ...) {
  insight::check_if_installed(c("see", "graphics"), "for overdispersion plots")
  obj_name <- attr(x, "object_name", exact = TRUE)
  model <- NULL

  if (!is.null(obj_name)) {
    model <- tryCatch(get(obj_name, envir = parent.frame()),
      error = function(e) NULL
    )
    if (is.null(model)) {
      # second try, global env
      model <- tryCatch(get(obj_name, envir = globalenv()),
        error = function(e) NULL
      )
    }
  }
  if (!is.null(model)) {
    x <- .diag_overdispersion(model)
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
  x$chisq_statistic <- sprintf("%.*f", digits, x$chisq_statistic)

  x$p_value <- pval <- round(x$p_value, digits = digits)
  if (x$p_value < .001) x$p_value <- "< 0.001"

  maxlen <- max(
    nchar(x$dispersion_ratio),
    nchar(x$chisq_statistic),
    nchar(x$p_value)
  )

  insight::print_color("# Overdispersion test\n\n", "blue")
  cat(sprintf("       dispersion ratio = %s\n", format(x$dispersion_ratio, justify = "right", width = maxlen)))
  cat(sprintf("  Pearson's Chi-Squared = %s\n", format(x$chisq_statistic, justify = "right", width = maxlen)))
  cat(sprintf("                p-value = %s\n\n", format(x$p_value, justify = "right", width = maxlen)))

  if (pval > 0.05) {
    message("No overdispersion detected.")
  } else {
    message("Overdispersion detected.")
  }

  invisible(orig_x)
}



# Overdispersion for classical models -----------------------------

#' @export
check_overdispersion.glm <- function(x, verbose = TRUE, ...) {
  # check if we have poisson
  info <- insight::model_info(x)
  if (!info$is_count && !info$is_binomial) {
    insight::format_error(
      "Overdispersion checks can only be used for models from Poisson families or binomial families with trials > 1."
    )
  }

  # check for Bernoulli
  if (info$is_bernoulli) {
    insight::format_error("Overdispersion checks cannot be used for Bernoulli models.")
  }

  if (info$is_binomial) {
    return(check_overdispersion.merMod(x, verbose = verbose, ...))
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
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(x))

  out
}

#' @export
check_overdispersion.fixest <- check_overdispersion.glm

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
check_overdispersion.merMod <- function(x, verbose = TRUE, ...) {
  # check if we have poisson or binomial
  info <- insight::model_info(x)
  if (!info$is_count && !info$is_binomial) {
    insight::format_error(
      "Overdispersion checks can only be used for models from Poisson families or binomial families with trials > 1."
    )
  }

  # check for Bernoulli
  if (info$is_bernoulli) {
    insight::format_error("Overdispersion checks cannot be used for Bernoulli models.")
  }

  rdf <- stats::df.residual(x)
  rp <- insight::get_residuals(x, type = "pearson")
  if (insight::is_empty_object(rp)) {
    Pearson.chisq <- NA
    prat <- NA
    pval <- NA
    rp <- NA
    if (isTRUE(verbose)) {
      insight::format_warning(
        "Cannot test for overdispersion, because pearson residuals are not implemented for models with zero-inflation or variable dispersion.",
        "Only the visual inspection using `plot(check_overdispersion(model))` is possible."
      )
    }
  } else {
    Pearson.chisq <- sum(rp^2)
    prat <- Pearson.chisq / rdf
    pval <- stats::pchisq(Pearson.chisq, df = rdf, lower.tail = FALSE)
  }

  out <- list(
    chisq_statistic = Pearson.chisq,
    dispersion_ratio = prat,
    residual_df = rdf,
    p_value = pval
  )

  class(out) <- c("check_overdisp", "see_check_overdisp")
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(x))

  out
}

#' @export
check_overdispersion.negbin <- check_overdispersion.merMod

#' @export
check_overdispersion.glmmTMB <- check_overdispersion.merMod
