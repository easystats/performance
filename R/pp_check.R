#' @title Posterior predictive checks for frequentist models
#' @name pp_check
#'
#' @description Posterior predictive checks for frequentist models.
#'
#' @param object A statistical model.
#' @param iterations The number of draws to simulate/bootstrap.
#' @param check_range Logical, if \code{TRUE}, includes a plot with the minimum
#'   value of the original response against the minimum values of the replicated
#'   responses, and the same for the maximum value. This plot helps judging whether
#'   the variation in the original data is captured by the model or not
#'   (\cite{Gelman et al. 2020, pp.163}). The minimum and maximum values of \code{y} should
#'   be inside the range of the related minimum and maximum values of \code{yrep}.
#' @param re_formula Formula containing group-level effects (random effects) to
#'   be considered in the simulated data. If \code{NULL} (default), condition
#'   on all random effects. If \code{NA} or \code{~0}, condition on no random
#'   effects. See \code{simulate()} in \strong{lme4}.
#' @param ... Passed down to \code{simulate()}.
#'
#' @return A data frame of simulated responses and the original response vector.
#'
#' @details Posterior predictive checks means \dQuote{simulating replicated data
#'   under the fitted model and then comparing these to the observed data}
#'   \cite{(Gelman and Hill, 2007, p. 158)}. Posterior predictive checks
#'   can be used to \dQuote{look for systematic discrepancies between real and
#'   simulated data} \cite{(Gelman et al. 2014, p. 169)}.
#'   \cr \cr
#'   An example how posterior predictive checks can also be used for model
#'   comparison is following plot (from \cite{Gabry et al. 2019, Figure 6}):
#'   \cr
#'   \if{html}{\cr \figure{pp_check.png}{options: width="90\%" alt="Posterior Predictive Check"} \cr}
#'   The model shown in the right panel (b) can simulate new data that are more
#'   similar to the observed outcome than the model in the left panel (a). Thus,
#'   model (b) is likely to be preferred over model (a).
#'
#' @note The default-method, \code{pp_check.default()} is in package \pkg{bayesplot}.
#' Thus, \pkg{performance} adds \code{pp_check()}-methods for different classes and
#' packages (like \code{lm}, \code{merMod}, \code{glmmTMB}, ...). However, since
#' it might be that not all model objects that have a \code{simulate()} function
#' are covered, and those objects probably can't be passed down to the default-method,
#' there is also a "generic" \code{posterior_predictive_check()} function (and
#' its alias \code{check_posterior_predictions()}), which just calls
#' \code{pp_check.lm()}. Thus, every model object that has a \code{simulate()}-method
#' should work with \code{posterior_predictive_check()}.
#'
#' @references \itemize{
#'   \item Gabry, J., Simpson, D., Vehtari, A., Betancourt, M., & Gelman, A. (2019). Visualization in Bayesian workflow. Journal of the Royal Statistical Society: Series A (Statistics in Society), 182(2), 389–402. https://doi.org/10.1111/rssa.12378
#'   \item Gelman, A., & Hill, J. (2007). Data analysis using regression and multilevel/hierarchical models. Cambridge; New York: Cambridge University Press.
#'   \item Gelman, A., Carlin, J. B., Stern, H. S., Dunson, D. B., Vehtari, A., & Rubin, D. B. (2014). Bayesian data analysis. (Third edition). CRC Press.
#'   \item Gelman, A., Hill, J., & Vehtari, A. (2020). Regression and Other Stories. Cambridge University Press.
#' }
#'
#' @examples
#' \dontrun{
#' library(performance)
#' model <- lm(Sepal.Length ~ Species * Petal.Width + Petal.Length, data = iris)
#' if (require("ggplot2") && require("see")) {
#'   pp_check(model)
#' }}
#' @importFrom stats simulate
#' @importFrom insight get_response
#' @export
pp_check <- function(object, ...) {
  UseMethod("pp_check")
}


#' @rdname pp_check
#' @export
pp_check.lm <- function(object, iterations = 50, check_range = FALSE, re_formula = NULL, ...) {
  out <- tryCatch(
    {
      stats::simulate(object, nsim = iterations, re.form = re_formula, ...)
    },
    error = function(e) { NULL }
  )

  if (is.null(out)) {
    stop(sprintf("Could not simulate responses. Maybe there is no 'simulate()' for objects of class '%s'?", class(object)[1]), call. = FALSE)
  }

  out$y <- insight::get_response(object)
  attr(out, "check_range") <- check_range
  class(out) <- c("performance_pp_check", "see_performance_pp_check", class(out))
  out
}

#' @export
pp_check.glm <- pp_check.lm

#' @export
pp_check.merMod <- pp_check.lm

#' @export
pp_check.MixMod <- pp_check.lm

#' @export
pp_check.glmmTMB <- pp_check.lm

#' @export
pp_check.glm.nb <- pp_check.lm

#' @export
pp_check.lme <- pp_check.lm

#' @export
pp_check.negbin <- pp_check.lm

#' @export
pp_check.polr <- pp_check.lm

#' @export
pp_check.wbm <- pp_check.lm

#' @export
pp_check.mle2 <- pp_check.lm

#' @export
pp_check.vlm <- pp_check.lm

#' @rdname pp_check
#' @export
posterior_predictive_check <- pp_check.lm

#' @rdname pp_check
#' @export
check_posterior_predictions <- pp_check.lm




# methods -----------------------


#' @export
print.performance_pp_check <- function(x, verbose = TRUE, ...) {
  original <- x$y
  replicated <- x[which(names(x) != "y")]

  if (min(replicated) > min(original)) {
    if (verbose) {
      warning("Minimum value of original data is not included in the replicated data. Model may not capture the variation of the data.", call. = FALSE)
    }
  }

  if (max(replicated) < max(original)) {
    if (verbose) {
      warning("Maximum value of original data is not included in the replicated data. Model may not capture the variation of the data.", call. = FALSE)
    }
  }

  if (requireNamespace("see", quietly = TRUE)) {
    NextMethod()
  }
  invisible(x)
}


#' @export
plot.performance_pp_check <- function(x, ...) {
  if (!requireNamespace("see", quietly = TRUE)) {
    stop("Package 'see' required to plot posterior predictive checks. Please install it.")
  }
  NextMethod()
}
