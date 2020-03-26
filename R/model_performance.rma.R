#' Performance of Meta-Analysis Models
#'
#' Compute indices of model performance for meta-analysis model from the \pkg{metafor} package.
#'
#' @param model A \code{rma} object as returned by \code{metafor::rma()}.
#' @param metrics Can be \code{"all"} or a character vector of metrics to be computed (some of \code{c("AIC", "BIC", "I2", "H2", "TAU2", "R2")}).
#' @param ... Arguments passed to or from other methods.
#' @inheritParams model_performance.lm
#'
#' @return A data frame (with one row) and one column per "index" (see \code{metrics}).
#'
#' @details \subsection{Indices of fit}{
#' \itemize{
#'   \item{\strong{AIC}} {Akaike's Information Criterion, see \code{\link[stats]{AIC}}}
#'   \item{\strong{BIC}} {Bayesian Information Criterion, see \code{\link[stats]{BIC}}}
#'    \item \strong{I2}: For a random effects model, \code{I2} estimates (in percent) how much of the total variability in the effect size estimates can be attributed to heterogeneity among the true effects. For a mixed-effects model, \code{I2} estimates how much of the unaccounted variability can be attributed to residual heterogeneity.
#'    \item \strong{H2}: For a random-effects model, \code{H2} estimates the ratio of the total amount of variability in the effect size estimates to the amount of sampling variability. For a mixed-effects model, \code{H2} estimates the ratio of the unaccounted variability in the effect size estimates to the amount of sampling variability.
#'    \item \strong{TAU2}: The amount of (residual) heterogeneity in the random or mixed effects model.
#'    \item \strong{R2}: Pseudo-R2-statistic, which indicates the amount of heterogeneity accounted for by the moderators included in a fixed-effects model.
#' }
#' See the documentation for \code{\link[metafor]{fitstats}}.
#' }
#'
#' @examples
#' if (require("metafor")) {
#'   data(dat.bcg)
#'   dat <- escalc(measure = "RR", ai = tpos, bi = tneg, ci = cpos, di = cneg, data = dat.bcg)
#'   model <- rma(yi, vi, data = dat, method= "REML")
#'   model_performance(model)
#' }
#' @export
model_performance.rma <- function(model, metrics = "all", verbose = TRUE, ...) {
  if (all(metrics == "all")) {
    metrics <- c("AIC", "BIC", "I2", "H2", "TAU2", "R2")
  } else if (all(metrics == "common")) {
    metrics <- c("AIC", "BIC", "I2")
  }

  out <- list()
  attrib <- list()
  s <- summary(model)

  if ("AIC" %in% toupper(metrics)) {
    out$AIC <- performance_aic(model)
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
  if ("R2" %in% toupper(metrics)) {
    R2 <- r2(model)
    if (!is.null(R2)) {
      attrib$r2 <- attributes(R2)
      out <- c(out, R2)
    }
  }

  out <- as.data.frame(out)
  row.names(out) <- NULL
  class(out) <- c("performance_model", class(out))

  # Add attributes
  attributes(out) <- c(attributes(out), attrib)

  out
}
