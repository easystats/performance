#' @title Posterior predictive checks
#' @name check_predictions
#'
#' @description Posterior predictive checks mean \dQuote{simulating replicated data
#'   under the fitted model and then comparing these to the observed data}
#'   \cite{(Gelman and Hill, 2007, p. 158)}. Posterior predictive checks
#'   can be used to \dQuote{look for systematic discrepancies between real and
#'   simulated data} \cite{(Gelman et al. 2014, p. 169)}.
#'
#'   \pkg{performance} provides posterior predictive check methods for a variety
#'   of frequentist models (e.g., `lm`, `merMod`, `glmmTMB`, ...). For Bayesian
#'   models, the model is passed to \code{\link[bayesplot:pp_check]{bayesplot::pp_check()}}.
#'
#' @param object A statistical model.
#' @param iterations The number of draws to simulate/bootstrap.
#' @param check_range Logical, if `TRUE`, includes a plot with the minimum
#'   value of the original response against the minimum values of the replicated
#'   responses, and the same for the maximum value. This plot helps judging whether
#'   the variation in the original data is captured by the model or not
#'   (\cite{Gelman et al. 2020, pp.163}). The minimum and maximum values of `y` should
#'   be inside the range of the related minimum and maximum values of `yrep`.
#' @param re_formula Formula containing group-level effects (random effects) to
#'   be considered in the simulated data. If `NULL` (default), condition
#'   on all random effects. If `NA` or `~0`, condition on no random
#'   effects. See `simulate()` in **lme4**.
#' @param ... Passed down to `simulate()`.
#'
#' @return A data frame of simulated responses and the original response vector.
#'
#' @details An example how posterior predictive checks can also be used for model
#'   comparison is Figure 6 from \cite{Gabry et al. 2019, Figure 6}.
#'   \cr
#'   \if{html}{\cr \figure{pp_check.png}{options: width="90\%" alt="Posterior Predictive Check"} \cr}
#'   The model shown in the right panel (b) can simulate new data that are more
#'   similar to the observed outcome than the model in the left panel (a). Thus,
#'   model (b) is likely to be preferred over model (a).
#'
#' @note  Every model object that has a `simulate()`-method should work with
#'   `check_predictions()`. If \pkg{bayesplot} (or a package that imports
#'   \pkg{bayesplot} such as \pkg{rstanarm} or \pkg{brms}) is loaded, `pp_check()`
#'   is also available as an alias for `check_predictions()`.
#'
#' @references \itemize{
#'   \item Gabry, J., Simpson, D., Vehtari, A., Betancourt, M., & Gelman, A. (2019). Visualization in Bayesian workflow. Journal of the Royal Statistical Society: Series A (Statistics in Society), 182(2), 389–402. https://doi.org/10.1111/rssa.12378
#'   \item Gelman, A., & Hill, J. (2007). Data analysis using regression and multilevel/hierarchical models. Cambridge; New York: Cambridge University Press.
#'   \item Gelman, A., Carlin, J. B., Stern, H. S., Dunson, D. B., Vehtari, A., & Rubin, D. B. (2014). Bayesian data analysis. (Third edition). CRC Press.
#'   \item Gelman, A., Hill, J., & Vehtari, A. (2020). Regression and Other Stories. Cambridge University Press.
#' }
#'
#' @examples
#' library(performance)
#' model <- lm(mpg ~ disp, data = mtcars)
#' if (require("see")) {
#'   check_predictions(model)
#' }
#' @export
check_predictions <- function(object, iterations = 50, check_range = FALSE, re_formula = NULL, ...) {
  if (isTRUE(insight::model_info(object, verbose = FALSE))) {
    UseMethod("pp_check")
  } else {
    pp_check.lm(object, iterations = iterations, check_range = check_range, re_formula = re_formula, ...)
  }
}


#' @exportS3Method bayesplot::pp_check lm
pp_check.lm <- function(object, iterations = 50, check_range = FALSE, re_formula = NULL, ...) {
  out <- tryCatch(
    {
      stats::simulate(object, nsim = iterations, re.form = re_formula, ...)
    },
    error = function(e) {
      NULL
    }
  )

  if (is.null(out)) {
    stop(sprintf("Could not simulate responses. Maybe there is no 'simulate()' for objects of class '%s'?", class(object)[1]), call. = FALSE)
  }

  out$y <- insight::get_response(object)
  attr(out, "check_range") <- check_range
  class(out) <- c("performance_pp_check", "see_performance_pp_check", class(out))
  out
}

#' @exportS3Method bayesplot::pp_check glm
pp_check.glm <- pp_check.lm

#' @exportS3Method bayesplot::pp_check merMod
pp_check.merMod <- pp_check.lm

#' @exportS3Method bayesplot::pp_check MixMod
pp_check.MixMod <- pp_check.lm

#' @exportS3Method bayesplot::pp_check glmmTMB
pp_check.glmmTMB <- pp_check.lm

#' @exportS3Method bayesplot::pp_check glm.nb
pp_check.glm.nb <- pp_check.lm

#' @exportS3Method bayesplot::pp_check lme
pp_check.lme <- pp_check.lm

#' @exportS3Method bayesplot::pp_check negbin
pp_check.negbin <- pp_check.lm

#' @exportS3Method bayesplot::pp_check polr
pp_check.polr <- pp_check.lm

#' @exportS3Method bayesplot::pp_check wbm
pp_check.wbm <- pp_check.lm

#' @exportS3Method bayesplot::pp_check mle2
pp_check.mle2 <- pp_check.lm

#' @exportS3Method bayesplot::pp_check lm
pp_check.vlm <- pp_check.lm

#' @rdname check_predictions
#' @export
posterior_predictive_check <- check_predictions

#' @rdname check_predictions
#' @export
check_posterior_predictions <- check_predictions




# methods -----------------------


#' @export
print.performance_pp_check <- function(x, verbose = TRUE, ...) {
  original <- x$y
  replicated <- x[which(names(x) != "y")]

  if (min(replicated) > min(original)) {
    if (verbose) {
      insight::print_color(
        insight::format_message("Warning: Minimum value of original data is not included in the replicated data.", "Model may not capture the variation of the data."),
        "red"
      )

    }
  }

  if (max(replicated) < max(original)) {
    if (verbose) {
      insight::print_color(
        insight::format_message("Warning: Maximum value of original data is not included in the replicated data.", "Model may not capture the variation of the data."),
        "red"
      )
    }
  }

  if (requireNamespace("see", quietly = TRUE)) {
    NextMethod()
  }
  invisible(x)
}


#' @export
plot.performance_pp_check <- function(x, ...) {
  insight::check_if_installed("see", "to plot posterior predictive checks")

  NextMethod()
}
