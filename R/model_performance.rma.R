#' Performance of Meta-Analysis Models
#'
#' Compute indices of model performance for meta-analysis model from the
#' \pkg{metafor} package.
#'
#' @param model A `rma` object as returned by `metafor::rma()`.
#' @param metrics Can be `"all"` or a character vector of metrics to be
#'   computed (some of `c("AIC", "BIC", "I2", "H2", "TAU2", "R2",
#'   "CochransQ", "QE", "Omnibus", "QM")`).
#' @param ... Arguments passed to or from other methods.
#' @inheritParams model_performance.lm
#' @inheritParams model_performance.merMod
#'
#' @return A data frame (with one row) and one column per "index" (see
#'   `metrics`).
#'
#' @details \subsection{Indices of fit}{
#'
#'   - **AIC** Akaike's Information Criterion, see `?stats::AIC`
#'
#'   - **BIC** {Bayesian Information Criterion, see `?stats::BIC`}
#'
#'    - **I2**: For a random effects model, `I2` estimates (in
#'    percent) how much of the total variability in the effect size estimates
#'    can be attributed to heterogeneity among the true effects. For a
#'    mixed-effects model, `I2` estimates how much of the unaccounted
#'    variability can be attributed to residual heterogeneity.
#'
#'    - **H2**: For a random-effects model, `H2` estimates the
#'    ratio of the total amount of variability in the effect size estimates to
#'    the amount of sampling variability. For a mixed-effects model, `H2`
#'    estimates the ratio of the unaccounted variability in the effect size
#'    estimates to the amount of sampling variability.
#'
#'    - **TAU2**: The amount of (residual) heterogeneity in the random
#'    or mixed effects model.
#'
#'    - **CochransQ (QE)**: Test for (residual) Heterogeneity. Without
#'    moderators in the model, this is simply Cochran's Q-test.
#'
#'    - **Omnibus (QM)**: Omnibus test of parameters.
#'
#'    - **R2**: Pseudo-R2-statistic, which indicates the amount of
#'    heterogeneity accounted for by the moderators included in a fixed-effects
#'    model.
#'
#' See the documentation for `?metafor::fitstats`.
#' }
#'
#' @examples
#' if (require("metafor")) {
#'   data(dat.bcg)
#'   dat <- escalc(measure = "RR", ai = tpos, bi = tneg, ci = cpos, di = cneg, data = dat.bcg)
#'   model <- rma(yi, vi, data = dat, method = "REML")
#'   model_performance(model)
#' }
#' @export
model_performance.rma <- function(model, metrics = "all", estimator = "ML", verbose = TRUE, ...) {
  if (all(metrics == "all")) {
    metrics <- c("AIC", "BIC", "I2", "H2", "TAU2", "COCHRANSQ", "OMNIBUS", "R2")
  } else if (all(metrics == "common")) {
    metrics <- c("AIC", "BIC", "I2")
  }

  out <- list()
  attrib <- list()
  s <- summary(model)

  if ("AIC" %in% toupper(metrics)) {
    out$AIC <- performance_aic(model, estimator = estimator)
  }

  if ("BIC" %in% toupper(metrics)) {
    out$BIC <- .get_BIC(model)
  }

  if ("I2" %in% toupper(metrics)) {
    out$I2 <- s$I2 / 100
  }

  if ("H2" %in% toupper(metrics)) {
    out$H2 <- s$H2
  }

  if ("TAU2" %in% toupper(metrics)) {
    out$TAU2 <- s$tau2
  }

  if (any(c("QE", "COCHRANSQ") %in% toupper(metrics))) {
    out$CochransQ <- s$QE
    out$p_CochransQ <- s$QEp
    out$df_error <- tryCatch(
      {
        stats::df.residual(model)
      },
      error = function(e) {
        NULL
      }
    )
  }

  if (any(c("QM", "OMNIBUS") %in% toupper(metrics))) {
    out$Omnibus <- s$QM
    out$p_Omnibus <- s$QMp
  }

  if ("R2" %in% toupper(metrics)) {
    R2 <- r2(model)
    if (!is.null(R2)) {
      attrib$r2 <- attributes(R2)
      out <- c(out, R2)
    }
  }

  out <- as.data.frame(insight::compact_list(out))
  row.names(out) <- NULL
  class(out) <- c("performance_model", class(out))

  # Add attributes
  attributes(out) <- c(attributes(out), attrib)

  out
}
