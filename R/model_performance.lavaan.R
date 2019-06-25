#' Performance of lavaan SEM / CFA Models
#'
#' Compute indices of model performance for SEM or CFA models from the \code{lavaan} package.
#'
#' @param model A \code{lavaan} model.
#' @param metrics Can be \code{"all"} or a character vector of metrics to be computed (some of \code{c("Chisq", "Chisq_DoF", "Chisq_p", "Baseline", "Baseline_DoF", "Baseline_p", "CFI", "TLI", "NNFI", "RFI", "NFI", "PNFI", "IFI", "RNI", "Loglikelihood", "AIC", "BIC", "BIC_adjusted", "RMSEA", "SMRM")}).
#' @param ... Arguments passed to or from other methods.
#'
#' @return A data frame (with one row) and one column per "index" (see \code{metrics}).
#'
#' @details See the documentation for \code{lavaan::fitmeasures}.
#'
#' @examples
#' \dontrun{
#' library(lavaan)
#'
#' # Confirmatory Factor Analysis (CFA) ---------
#'
#' structure <- ' visual  =~ x1 + x2 + x3
#'                textual =~ x4 + x5 + x6
#'                speed   =~ x7 + x8 + x9 '
#' model <- lavaan::cfa(structure, data=HolzingerSwineford1939)
#' model_performance(model)
#' }
#'
#' @export
model_performance.lavaan <- function(model, metrics = "all", ...) {

  if (!requireNamespace("lavaan", quietly = TRUE)) {
    stop("Package `lavaan` required. Please install it by running `install.packages(lavaan)`.", call. = FALSE)
  }

  measures <-  as.data.frame(t(as.data.frame(lavaan::fitmeasures(model, ...))))
  row.names(measures) <- NULL

  out <- data.frame(
    "Chisq" = measures$chisq,
    "Chisq_DoF" = measures$df,
    "Chisq_p" = measures$pvalue,
    "Baseline" = measures$baseline.chisq,
    "Baseline_DoF" = measures$baseline.df,
    "Baseline_p" = measures$baseline.pvalue,
    "CFI" = measures$cfi,
    "TLI" = measures$tli,
    "NNFI" = measures$nnfi,
    "RFI" = measures$rfi,
    "NFI" = measures$nfi,
    "PNFI" = measures$pnfi,
    "IFI" = measures$ifi,
    "RNI" = measures$rni,
    "Loglikelihood" = measures$logl,
    "AIC" = measures$aic,
    "BIC" = measures$bic,
    "BIC_adjusted" = measures$bic2,
    "RMSEA" = measures$rmsea,
    "SMRM" = measures$srmr
  )

  if (all(metrics == "all")) {
    metrics <- names(out)
  }

  out[, metrics]
}