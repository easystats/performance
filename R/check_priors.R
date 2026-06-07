#' @title Prior predictive checks
#' @name check_priors
#'
#' @description
#' Simulates from the prior marginal distribution of the data to assess the
#' consistency of the chosen priors with domain knowledge (*Gabry et al. 2019*)
#' and creates a visualization from the prior predictive checks.
#'
#' @details
#' Assessing priors based on the prior marginal distribution for the data
#' provides several methodological advantages:
#' * It reflects the interplay between the prior distribution on the parameters
#'   and the likelihood.
#' * It is a vital component of understanding how prior distributions actually
#'   work for a given problem.
#' * It explicitly reflects the idea that we cannot fully understand the prior
#'   by fixing all except one parameter and assessing the effect of the
#'   unidimensional marginal prior.
#' * Instead, we need to assess the effect of the prior as a multivariate
#'   distribution.
#' * The prior distribution over the data enables us to extend the concept of a
#'   weakly informative prior to be more aware of the role of the
#'   likelihood.
#'
#' A prior leads to a weakly informative joint prior data-generating process if
#' draws from the prior data-generating distribution could represent any data
#' set that could plausibly be observed. Furthermore, there should be
#' no mass on completely implausible data sets. Generating simulated
#' data sets can be used to investigate the variability and multivariate
#' structure of the distribution.
#'
#' @param model A Bayesian model of class `stanreg` or `brmsfit`. Note that
#' this model should include draws from the prior predictive distribution. This
#' can be achieved, e.g., by using [`bayestestR::unupdate()`] on the fitted
#' model, or setting `prior_PD = TRUE` (when using **rstanarm**) or
#' `sample_prior = "only"` (when using **brms**).
#' @param predictors Character vector with names of one or more model predictors
#' for which prior predictive checks should be visualized.
#' @param ... Currently not used.
#'
#' @return A data frame of simulated responses and the original response vector.
#'
#' @references
#' Gabry, J., Simpson, D., Vehtari, A., Betancourt, M., & Gelman, A. (2019).
#' Visualization in Bayesian Workflow. Journal of the Royal Statistical Society
#' Series A: Statistics in Society, 182(2), 389–402. \doi{10.1111/rssa.12378}
#'
#' @examplesIf insight::check_if_installed("see", minimum_version = "0.9.1", quietly = TRUE)
#' \dontrun{
#' # check_priors(model)
#' }
#' @export
check_priors <- function(model = NULL, ...) {
  UseMethod("check_priors")
}

#' @export
check_priors.default <- function(model = NULL, ...) {
  insight::format_error("Prior predictive checks are not implemented for this model.")
}

#' @rdname check_priors
#' @export
check_priors.stanreg <- function(model = NULL, predictors = NULL, ...) {
  # sanity check
  if (is.null(predictors)) {
    insight::format_error("Argument `predictors` is required and cannot be `NULL`.")
  }

  x <- list(model = model)
  attr(x, "predictors") <- predictors
  class(x) <- c("performance_check_priors", "see_check_priors")

  graphics::plot(x)
}

#' @export
check_priors.brmsfit <- check_priors.stanreg

# methods ----------------------------------

#' @export
plot.performance_check_priors <- function(x, ...) {
  insight::check_if_installed("see", "for prior predictive plots")
  NextMethod()
}
