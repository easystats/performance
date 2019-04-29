#' Performance of Regression Models
#'
#' Compute indices of model performance for regression models.
#'
#' @param model A model.
#' @param metrics Can be \code{"all"} or a character vector of metrics to be computed (some of \code{c("AIC", "BIC", "R2", "RMSE", "LOGLOSS", "EPCP")}).
#' @param ... Arguments passed to or from other methods.
#'
#' @return A data frame (with one row) and one column per "index" (see \code{metrics}).
#'
#' @details Depending on \code{model}, following indices are computed:
#' \decribe{
#'   \item{\strong{AIC}}{Akaike's Information Criterion, see \code{\link[stats]{AIC}}}
#'   \item{\strong{BIC}}{Bayesian Information Criterion, see \code{\link[stats]{BIC}}}
#'   \item{\strong{R2}}{r-squared value, see \code{\link{r2}}}
#'   \item{\strong{R2_adj}}{adjusted r-squared, see \code{\link{r2}}}
#'   \item{\strong{RMSE}}{root mean squared error, see \code{\link{rmse}}}
#'   \item{\strong{LOGLOSS}}{Log-loss, see \code{\link{log_loss}}}
#'   \item{\strong{EPCP}}{expected percentage of correct predictions, see \code{\link{correct_predictions}}}
#' }
#'
#' @examples
#' model <- lm(mpg ~ wt + cyl, data = mtcars)
#' model_performance(model)
#'
#' model <- glm(vs ~ wt + mpg, data = mtcars, family = "binomial")
#' model_performance(model)
#'
#' @importFrom insight model_info
#' @importFrom stats AIC BIC
#' @export
model_performance.lm <- function(model, metrics = "all", ...) {
  if (all(metrics == "all")) {
    metrics <- c("AIC", "BIC", "R2", "R2_adj", "RMSE", "LOGLOSS", "EPCP")
  }

  mi <- insight::model_info(model)

  out <- list()
  if ("AIC" %in% metrics) {
    out$AIC <- stats::AIC(model)
  }
  if ("BIC" %in% metrics) {
    out$BIC <- stats::BIC(model)
  }
  if ("R2" %in% metrics) {
    out <- c(out, r2(model))
  }
  if ("RMSE" %in% metrics) {
    out$RMSE <- rmse(model)
  }
  if (("LOGLOSS" %in% metrics) && mi$is_binomial) {
    out$LOGLOSS <- log_loss(model)
  }

  if (("EPCP" %in% metrics) && mi$is_binomial) {
    out$EPCP <- correct_predictions(model)$epcp
  }

  # TODO: What with sigma and deviance?

  out <- as.data.frame(out)
  row.names(out) <- NULL
  out
}

#' @rdname model_performance.lm
#' @export
model_performance.glm <- model_performance.lm

#' @export
model_performance.betareg <- model_performance.lm

#' @export
model_performance.censReg <- model_performance.lm

#' @export
model_performance.clm <- model_performance.lm

#' @export
model_performance.clm2 <- model_performance.lm

#' @export
model_performance.coxph <- model_performance.lm

#' @export
model_performance.felm <- model_performance.lm

#' @export
model_performance.iv_robust <- model_performance.lm

#' @export
model_performance.mlogit <- model_performance.lm

#' @export
model_performance.multinom <- model_performance.lm

#' @export
model_performance.plm <- model_performance.lm

#' @export
model_performance.polr <- model_performance.lm

#' @export
model_performance.survreg <- model_performance.lm

#' @export
model_performance.svyglm <- model_performance.lm

#' @export
model_performance.truncreg <- model_performance.lm

#' @export
model_performance.vglm <- model_performance.lm
