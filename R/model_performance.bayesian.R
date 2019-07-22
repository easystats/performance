#' Performance of Bayesian Models
#'
#' Compute indices of model performance for (general) linear models.
#'
#' @param model Object of class \code{stanreg} or \code{brmsfit}.
#' @param metrics Can be \code{"all"} or a character vector of metrics to be computed (some of \code{c("LOOIC", "WAIC", "R2", "R2_adj", "RMSE", "LOGLOSS", "SCORE")}).
#' @param ... Arguments passed to or from other methods.
#'
#' @return A data frame (with one row) and one column per "index" (see \code{metrics}).
#'
#' @details Depending on \code{model}, following indices are computed:
#' \itemize{
#'   \item{\strong{ELPD}} {expected log predictive density, see \code{\link{looic}}}
#'   \item{\strong{LOOIC}} {leave-one-out cross-validation (LOO) information criterion, see \code{\link{looic}}}
#'   \item{\strong{WAIC}} {widely applicable information criterion, see \code{loo::waic}}
#'   \item{\strong{R2}} {r-squared value, see \code{\link{r2}}}
#'   \item{\strong{R2_LOO_adjusted}} {adjusted r-squared, see \code{\link{r2}}}
#'   \item{\strong{RMSE}} {root mean squared error, see \code{\link{performance_rmse}}}
#'   \item{\strong{LOGLOSS}} {Log-loss, see \code{\link{performance_logloss}}}
#'   \item{\strong{SCORE_LOG}} {score of logarithmic proper scoring rule, see \code{\link{performance_score}}}
#'   \item{\strong{SCORE_SPHERICAL}} {score of spherical proper scoring rule, see \code{\link{performance_score}}}
#'   \item{\strong{PCP}} {percentage of correct predictions, see \code{\link{performance_pcp}}}
#' }
#'
#' @examples
#' library(rstanarm)
#'
#' model <- stan_glm(mpg ~ wt + cyl, data = mtcars, chains = 1, iter = 500)
#' model_performance(model)
#'
#' model <- stan_glmer(
#'   mpg ~ wt + cyl + (1 | gear),
#'   data = mtcars,
#'   chains = 1,
#'   iter = 500
#' )
#' model_performance(model)
#'
#' @seealso \link{r2_bayes}
#' @references Gelman, A., Goodrich, B., Gabry, J., & Vehtari, A. (2018). R-squared for Bayesian regression models. The American Statistician, The American Statistician, 1-6.
#'
#' @importFrom insight find_algorithm
#' @importFrom bayestestR map_estimate hdi
#' @importFrom stats AIC BIC mad median sd setNames
#' @export
model_performance.stanreg <- function(model, metrics = "all", ...) {
  if (all(metrics == "all")) {
    metrics <- c("LOOIC", "WAIC", "R2", "R2_adjusted", "RMSE", "LOGLOSS", "SCORE")
  }

  algorithm <- insight::find_algorithm(model)
  if (algorithm$algorithm != "sampling") {
    warning("`model_performance()` only possible for models fit using the 'sampling' algorithm.", call. = FALSE)
    return(NULL)
  }

  if (!requireNamespace("loo", quietly = TRUE)) {
    stop("Package `loo` required for this function to work. Please install it.")
  }

  mi <- insight::model_info(model)

  out <- list()
  if ("LOOIC" %in% c(metrics)) {
    out <- append(out, looic(model))
  }
  if ("WAIC" %in% c(metrics)) {
    out$WAIC <- suppressWarnings(loo::waic(model)$estimates["waic", "Estimate"])
  }
  if ("R2" %in% c(metrics)) {
    r2 <- .r2_posterior(model)
    if (!is.null(r2) && !all(is.na(r2))) {
      out <- c(out, .summarize_r2_bayes(r2$R2_Bayes, name = "R2"))
      if ("R2_Bayes_marginal" %in% names(r2)) {
        out <- c(out, .summarize_r2_bayes(r2$R2_Bayes_marginal, name = "R2_marginal"))
      }
    }
  }
  if ("R2_adjusted" %in% c(metrics) && mi$is_linear) {
    out$R2_LOO_adjusted <- r2_loo(model)
  }
  if ("RMSE" %in% c(metrics) && !mi$is_ordinal && !mi$is_categorical) {
    out$RMSE <- performance_rmse(model)
  }
  if (("LOGLOSS" %in% metrics) && mi$is_binomial) {
    out$LOGLOSS <- performance_logloss(model)
  }
  if (("SCORE" %in% metrics) && (mi$is_binomial || mi$is_count)) {
    .scoring_rules <- performance_score(model)
    if (!is.na(.scoring_rules$logarithmic)) out$SCORE_LOG <- .scoring_rules$logarithmic
    if (!is.na(.scoring_rules$spherical)) out$SCORE_SPHERICAL <- .scoring_rules$spherical
  }

  # TODO: What with sigma and deviance?

  out <- as.data.frame(out)
  row.names(out) <- NULL
  out
}


#' @export
model_performance.brmsfit <- model_performance.stanreg


#' @keywords internal
.summarize_r2_bayes <- function(r2_posterior, name = "R2_") {
  out <- list(mean(r2_posterior), stats::sd(r2_posterior))
  names(out) <- paste0(name, c("", "_SE"))
  out
}
