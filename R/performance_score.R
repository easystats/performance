#' @title Proper Scoring Rules
#' @name performance_score
#'
#' @description Calculates the logarithmic, quadratic/Brier and spherical score
#'   from a model with binary or count outcome.
#'
#' @param model Model with binary or count outcome.
#' @param ... Currently not used.
#' @inheritParams model_performance.lm
#'
#' @return A list with three elements, the logarithmic, quadratic/Brier and spherical score.
#'
#' @details Proper scoring rules can be used to evaluate the quality of model
#' predictions and model fit. \code{performance_score()} calculates the logarithmic,
#' quadratic/Brier and spherical scoring rules. The spherical rule takes values
#' in the interval \code{[0, 1]}, with values closer to 1 indicating a more
#' accurate model, and the logarithmic rule in the interval \code{[-Inf, 0]},
#' with values closer to 0 indicating a more accurate model.
#' \cr \cr
#' For \code{stan_lmer()} and \code{stan_glmer()} models, the predicted values
#' are based on \code{posterior_predict()}, instead of \code{predict()}. Thus,
#' results may differ more than expected from their non-Bayesian counterparts
#' in \pkg{lme4}.
#'
#' @references Carvalho, A. (2016). An overview of applications of proper scoring rules. Decision Analysis 13, 223–242. \doi{10.1287/deca.2016.0337}
#'
#' @note Code is partially based on \href{https://drizopoulos.github.io/GLMMadaptive/reference/scoring_rules.html}{GLMMadaptive::scoring_rules()}.
#'
#' @seealso \code{\link[=performance_logloss]{performance_logloss()}}
#'
#' @examples
#' ## Dobson (1990) Page 93: Randomized Controlled Trial :
#' counts <- c(18, 17, 15, 20, 10, 20, 25, 13, 12)
#' outcome <- gl(3, 1, 9)
#' treatment <- gl(3, 3)
#' model <- glm(counts ~ outcome + treatment, family = poisson())
#'
#' performance_score(model)
#'
#' \dontrun{
#' library(glmmTMB)
#' data(Salamanders)
#' model <- glmmTMB(
#'   count ~ spp + mined + (1 | site),
#'   zi =  ~ spp + mined,
#'   family = nbinom2(),
#'   data = Salamanders
#' )
#'
#' performance_score(model)
#' }
#'
#' @importFrom insight get_response model_info
#' @importFrom stats dbinom dpois dnbinom ppois pnbinom
#' @export
performance_score <- function(model, verbose = TRUE) {
  minfo <- insight::model_info(model)

  if (minfo$is_ordinal) {
    if (verbose) insight::print_color("Can't calculate proper scoring rules for ordinal or cumulative link models.\n", "red")
    return(list(logarithmic = NA, quadratic = NA, spherical = NA))
  }

  prob_fun <- if (minfo$is_binomial) {
    function(x, mean, pis, n) stats::dbinom(x, size = n, prob = mean)
  } else if (minfo$is_poisson && !minfo$is_zero_inflated) {
    function(x, mean, pis, n) stats::dpois(x, lambda = mean)
  } else if (minfo$is_negbin && !minfo$is_zero_inflated) {
    function(x, mean, pis, n) dnbinom(x, mu = mean, size = exp(.dispersion_parameter(model, minfo)))
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
  resp <- .factor_to_numeric(insight::get_response(model))
  p_y <- prob_fun(resp, mean = pr$pred, pis = pr$pred_zi, sum(resp))

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



#' @importFrom stats residuals df.residual
.dispersion_parameter <- function(model, minfo) {
  if (inherits(model, "MixMod")) {
    model$phis
  } else if (inherits(model, "glmmTMB")) {
    if (minfo$is_zero_inflated) {
      if (!requireNamespace("glmmTMB")) {
        stop("Package 'glmmTMB' required for this function work. Please install it.")
      }
      glmmTMB::getME(model, "theta")
    } else {
      sum(stats::residuals(model , type = "pearson")^2) / stats::df.residual(model)
    }
  } else {
    tryCatch({
      sum(stats::residuals(model , type = "pearson")^2) / stats::df.residual(model)
    },
    error = function(e) {
      0
    })
  }
}



#' @importFrom stats predict
.predict_score_y <- function(model) {
  pred <- NULL
  pred_zi <- NULL

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
    pred <- colMeans(rstanarm::posterior_predict(model))
  } else {
    pred <- stats::predict(model, type = "response")
  }

  list(pred = pred, pred_zi = pred_zi)
}