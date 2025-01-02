#' @title Proper Scoring Rules
#' @name performance_score
#'
#' @description Calculates the logarithmic, quadratic/Brier and spherical score
#'   from a model with binary or count outcome.
#'
#' @param model Model with binary or count outcome.
#' @param ... Arguments from other functions, usually only used internally.
#' @inheritParams model_performance.lm
#'
#' @return A list with three elements, the logarithmic, quadratic/Brier and spherical score.
#'
#' @details Proper scoring rules can be used to evaluate the quality of model
#' predictions and model fit. `performance_score()` calculates the logarithmic,
#' quadratic/Brier and spherical scoring rules. The spherical rule takes values
#' in the interval `[0, 1]`, with values closer to 1 indicating a more
#' accurate model, and the logarithmic rule in the interval `[-Inf, 0]`,
#' with values closer to 0 indicating a more accurate model.
#'
#' For `stan_lmer()` and `stan_glmer()` models, the predicted values
#' are based on `posterior_predict()`, instead of `predict()`. Thus,
#' results may differ more than expected from their non-Bayesian counterparts
#' in **lme4**.
#'
#' @references
#' Carvalho, A. (2016). An overview of applications of proper scoring rules.
#' Decision Analysis 13, 223–242. \doi{10.1287/deca.2016.0337}
#'
#' @note
#' Code is partially based on
#' [GLMMadaptive::scoring_rules()](https://drizopoulos.github.io/GLMMadaptive/reference/scoring_rules.html).
#'
#' @seealso [`performance_logloss()`]
#'
#' @examplesIf require("glmmTMB")
#' ## Dobson (1990) Page 93: Randomized Controlled Trial :
#' counts <- c(18, 17, 15, 20, 10, 20, 25, 13, 12)
#' outcome <- gl(3, 1, 9)
#' treatment <- gl(3, 3)
#' model <- glm(counts ~ outcome + treatment, family = poisson())
#'
#' performance_score(model)
#' \donttest{
#' data(Salamanders, package = "glmmTMB")
#' model <- glmmTMB::glmmTMB(
#'   count ~ spp + mined + (1 | site),
#'   zi = ~ spp + mined,
#'   family = nbinom2(),
#'   data = Salamanders
#' )
#'
#' performance_score(model)
#' }
#' @export
performance_score <- function(model, verbose = TRUE, ...) {
  # check special case
  if (inherits(model, c("logitor", "logitmfx", "probitmfx", "negbinirr", "negbinmfx", "poissonirr", "poissonmfx"))) {
    model <- model$fit
  }

  minfo <- list(...)$model_info
  if (is.null(minfo)) {
    minfo <- suppressWarnings(insight::model_info(model, verbose = FALSE))
  }

  if (minfo$is_ordinal || minfo$is_multinomial) {
    if (verbose) {
      insight::format_alert("Can't calculate proper scoring rules for ordinal, multinomial or cumulative link models.")
    }
    return(list(logarithmic = NA, quadratic = NA, spherical = NA))
  }

  resp <- insight::get_response(model, verbose = verbose)

  if (!is.null(ncol(resp)) && ncol(resp) > 1) {
    if (verbose) {
      insight::format_alert("Can't calculate proper scoring rules for models without integer response values.")
    }
    return(list(logarithmic = NA, quadratic = NA, spherical = NA))
  }

  prob_fun <- if (minfo$is_binomial) {
    function(x, mean, pis, n) stats::dbinom(x, size = n, prob = mean)
  } else if (minfo$is_poisson && !minfo$is_zero_inflated) {
    function(x, mean, pis, n) stats::dpois(x, lambda = mean)
  } else if (minfo$is_negbin && !minfo$is_zero_inflated) {
    function(x, mean, pis, n) stats::dnbinom(x, mu = mean, size = exp(.dispersion_parameter(model, minfo)))
  } else if (minfo$is_poisson && minfo$is_zero_inflated && !minfo$is_hurdle) {
    function(x, mean, pis, n) {
      ind0 <- x == 0
      out <- (1 - pis) * stats::dpois(x, lambda = mean / (1 - pis))
      out[ind0] <- pis[ind0] + out[ind0]
      out
    }
  } else if (minfo$is_zero_inflated && minfo$is_negbin && !minfo$is_hurdle) {
    function(x, mean, pis, n) {
      ind0 <- x == 0
      out <- (1 - pis) * stats::dnbinom(x, mu = mean / (1 - pis), size = exp(.dispersion_parameter(model, minfo)))
      out[ind0] <- pis[ind0] + out[ind0]
      out
    }
  } else if (minfo$is_hurdle && minfo$is_poisson) {
    function(x, mean, pis, n) {
      ind0 <- x == 0
      trunc_zero <- stats::dpois(x, lambda = mean) / stats::ppois(0, lambda = mean, lower.tail = FALSE)
      out <- (1 - pis) * trunc_zero
      out[ind0] <- pis[ind0]
      out
    }
  } else if (minfo$is_hurdle && minfo$is_negbin) {
    function(x, mean, pis, n) {
      ind0 <- x == 0
      trunc_zero <- stats::dnbinom(x, mu = mean, size = exp(.dispersion_parameter(model, minfo))) /
        stats::pnbinom(0, mu = mean, size = exp(.dispersion_parameter(model, minfo)), lower.tail = FALSE)
      out <- (1 - pis) * trunc_zero
      out[ind0] <- pis[ind0]
      out
    }
  }

  pr <- .predict_score_y(model)
  resp <- if (minfo$is_binomial) {
    .recode_to_zero(resp)
  } else {
    datawizard::to_numeric(resp, dummy_factors = FALSE, preserve_levels = TRUE)
  }
  p_y <- .safe(suppressWarnings(prob_fun(resp, mean = pr$pred, pis = pr$pred_zi, sum(resp))))

  if (is.null(p_y) || all(is.na(p_y))) {
    if (verbose) {
      insight::format_alert("Can't calculate proper scoring rules for this model.")
    }
    return(list(logarithmic = NA, quadratic = NA, spherical = NA))
  }

  quadrat_p <- sum(p_y^2)

  structure(
    class = "performance_score",
    list(
      logarithmic = mean(log(p_y)),
      quadratic = mean(2 * p_y + quadrat_p),
      spherical = mean(p_y / sqrt(quadrat_p))
    )
  )
}


# methods -----------------------------------

#' @export
as.data.frame.performance_score <- function(x, row.names = NULL, ...) {
  data.frame(
    logarithmic = x$logarithmic,
    quadratic = x$quadratic,
    spherical = x$spherical,
    stringsAsFactors = FALSE,
    row.names = row.names,
    ...
  )
}


#' @export
print.performance_score <- function(x, ...) {
  # headline
  insight::print_color("# Proper Scoring Rules\n\n", "blue")

  results <- format(
    c(
      sprintf("%.4f", x$logarithmic),
      sprintf("%.4f", x$quadratic),
      sprintf("%.4f", x$spherical)
    ),
    justify = "right"
  )

  cat(sprintf("logarithmic: %s\n", results[1]))
  cat(sprintf("  quadratic: %s\n", results[2]))
  cat(sprintf("  spherical: %s\n", results[3]))

  invisible(x)
}


# utilities ---------------------------------

.dispersion_parameter <- function(model, minfo) {
  if (inherits(model, "MixMod")) {
    model$phis
  } else if (inherits(model, "glmmTMB")) {
    if (minfo$is_zero_inflated) {
      insight::check_if_installed("glmmTMB")
      glmmTMB::getME(model, "theta")
    } else {
      sum(stats::residuals(model, type = "pearson")^2) / stats::df.residual(model)
    }
  } else {
    .safe(sum(stats::residuals(model, type = "pearson")^2) / stats::df.residual(model), 0)
  }
}


.predict_score_y <- function(model) {
  pred <- NULL
  pred_zi <- NULL

  tryCatch(
    if (inherits(model, "MixMod")) {
      pred <- stats::predict(model, type = "subject_specific")
      pred_zi <- if (!is.null(model$gammas)) attr(pred, "zi_probs")
    } else if (inherits(model, "glmmTMB")) {
      pred <- stats::predict(model, type = "response")
      pred_zi <- stats::predict(model, type = "zprob")
    } else if (inherits(model, c("hurdle", "zeroinfl"))) {
      pred <- stats::predict(model, type = "response")
      pred_zi <- stats::predict(model, type = "zero")
    } else if (inherits(model, c("clm", "clm2", "clmm"))) {
      pred <- stats::predict(model)
    } else if (all(inherits(model, c("stanreg", "lmerMod"), which = TRUE)) > 0) {
      insight::check_if_installed("rstanarm")
      pred <- colMeans(rstanarm::posterior_predict(model))
    } else {
      pred <- stats::predict(model, type = "response")
    },
    error = function(e) {
      NULL
    }
  )

  list(pred = pred, pred_zi = pred_zi)
}
