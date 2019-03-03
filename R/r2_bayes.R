#' R2 for Bayesian regressions.
#'
#' Compute R2 for Bayesian models. For mixed models (including a random part), it additionally computes the R2 related to the fixed effects only.
#'
#' @param model A Bayesian regression model.
#' @param robust Logical, if \code{TRUE}, the median instead of mean is used to
#'   calculate the central tendency of the variances.
#'
#' @examples
#' \dontrun{
#' library(rstanarm)
#'
#' model <- rstanarm::stan_glm(mpg ~ wt + cyl, data = mtcars)
#' r2_bayes(model)
#'
#' model <- rstanarm::stan_lmer(Petal.Length ~ Petal.Width + (1 | Species), data = iris)
#' r2_bayes(model)
#'
#' library(brms)
#'
#' model <- brms::brm(mpg ~ wt + cyl, data = mtcars)
#' r2_bayes(model)
#'
#' model <- brms::brm(Petal.Length ~ Petal.Width + (1 | Species), data = iris)
#' r2_bayes(model)
#' }
#'
#' @references Gelman, A., Goodrich, B., Gabry, J., & Vehtari, A. (2018). R-squared for Bayesian regression models. The American Statistician, The American Statistician, 1-6.
#'
#' @importFrom stats median
#' @export
r2_bayes <- function(model, robust = FALSE) {
  r2_bayesian <- .r2_posterior(model)
  lapply(r2_bayesian, ifelse(robust, stats::median, mean))
}

#' @keywords internal
.r2_posterior <- function(model) {
  if (!requireNamespace("rstantools", quietly = TRUE)) {
    stop("This function needs `rstantools` to be installed.")
  }

  if (insight::model_info(model)$is_mixed) {
    list(
      "R2_Bayes" = as.vector(rstantools::bayes_R2(model, re.form = NULL, re_formula = NULL, summary = FALSE)),
      "R2_Bayes_marginal" = as.vector(rstantools::bayes_R2(model, re.form = NA, re_formula = NA, summary = FALSE))
    )
  } else {
    list("R2_Bayes" = as.vector(rstantools::bayes_R2(model, summary = FALSE)))
  }
}