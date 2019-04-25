#' Performance of (Generalized) Linear Models
#'
#' Compute indices of model performance for (generalized) linear models.
#'
#' @param model Object of class \code{lm} or \code{glm}.
#' @param metrics Can be \code{"all"} or a character vector of metrics to be computed (some of \code{c("AIC", "BIC", "R2", "RMSE")}).
#' @param ... Arguments passed to or from other methods.
#'
#' @return A data frame (with one row) and one column per "index" (see \code{metrics}).
#'
#' @examples
#' model <- lm(mpg ~ wt + cyl, data = mtcars)
#' model_performance(model)
#'
#' model <- glm(vs ~ wt + mpg, data = mtcars, family = "binomial")
#' model_performance(model)
#'
#' @importFrom stats AIC BIC
#' @importFrom insight model_info
#' @export
model_performance.lm <- function(model, metrics = "all", ...) {
  if (all(metrics == "all")) {
    metrics <- c("AIC", "BIC", "R2", "R2_adj", "RMSE")
  }

  minfo <- insight::model_info(model)

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
  if ("RMSE" %in% metrics && minfo$is_linear) {
    out$RMSE <- rmse(model)
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
