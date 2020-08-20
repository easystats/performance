#' @title Posterior predictive checks for frequentist models
#' @name pp_check
#'
#' @description Posterior predictive checks for frequentist models.
#'
#' @param object A statistical model.
#' @param iterations The number of draws to simulate/bootstrap.
#' @param ... Not used.
#'
#' @return A data frame
#'
#' @details Posterior predictive checks means \dQuote{simulating replicated data
#'   under the fitted model and then comparing these to the observed data}
#'   \cite{(Gelman and Hill, 2007, p. 158)}. Posterior predictive checks
#'   can be used to \dQuote{look for systematic discrepancies between real and
#'   simulated data}  \cite{(Gelman et al. 2014, p. 169)}.
#'
#' @references \itemize{
#'   \item Gelman, A., & Hill, J. (2007). Data analysis using regression and multilevel/hierarchical models. Cambridge; New York: Cambridge University Press.
#'   \item Gelman, A., Carlin, J. B., Stern, H. S., Dunson, D. B., Vehtari, A., & Rubin, D. B. (2014). Bayesian data analysis. (Third edition). CRC Press.
#' }
#'
#' @examples
#' model <- lm(Sepal.Length ~ Species * Petal.Width + Petal.Length, data = iris)
#' if (require("ggplot2") && require("see")) {
#'   pp_check(model)
#' }
#' @export
pp_check <- function(object, ...) {
  UseMethod("pp_check")
}


#' @export
pp_check.default <- function(object, iterations = 1000, ...) {
  out <- tryCatch(
    {
      stats::simulate(object, nsim = iterations)
    },
    error = function(e) { NULL }
  )

  if (is.null(out)) {
    stop(sprintf("Could not simulate responses. Maybe there is no 'simulate()' for objects of class '%s'?", class(object)[1]), call. = FALSE)
  }

  out$y <- insight::get_response(object)
  class(out) <- c("performance_pp_check", "see_performance_pp_check", class(out))
  out
}


#' @export
print.performance_pp_check <- function(x, ...) {
  if (!requireNamespace("see", quietly = TRUE)) {
    stop("Package 'see' required to plot posterior predictive checks Please install it.")
  }
  NextMethod()
}
