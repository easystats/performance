#' Performance of lavaan SEM / CFA Models
#'
#' Compute indices of model performance for SEM or CFA models from the
#' \pkg{lavaan} package.
#'
#' @param model A \pkg{lavaan} model.
#' @param metrics Can be `"all"` or a character vector of metrics to be
#'   computed (some of `c("Chi2", "Chi2_df", "p_Chi2", "Baseline",
#'   "Baseline_df", "p_Baseline", "GFI", "AGFI", "NFI", "NNFI", "CFI",
#'   "RMSEA", "RMSEA_CI_low", "RMSEA_CI_high", "p_RMSEA", "RMR", "SRMR",
#'   "RFI", "PNFI", "IFI", "RNI", "Loglikelihood", "AIC", "BIC",
#'   "BIC_adjusted")`).
#' @param verbose Toggle off warnings.
#' @param ... Arguments passed to or from other methods.
#'
#' @return A data frame (with one row) and one column per "index" (see
#'   `metrics`).
#'
#' @details \subsection{Indices of fit}{
#' \itemize{
#'    \item **Chisq**: The model Chi-squared assesses overall fit and the
#'    discrepancy between the sample and fitted covariance matrices. Its p-value
#'    should be > .05 (i.e., the hypothesis of a perfect fit cannot be
#'    rejected). However, it is quite sensitive to sample size.
#'
#'    \item **GFI/AGFI**: The (Adjusted) Goodness of Fit is the proportion
#'    of variance accounted for by the estimated population covariance.
#'    Analogous to R2. The GFI and the AGFI should be > .95 and > .90,
#'    respectively.
#'
#'    \item **NFI/NNFI/TLI**: The (Non) Normed Fit Index. An NFI of 0.95,
#'    indicates the model of interest improves the fit by 95\% relative to the
#'    null model. The NNFI (also called the Tucker Lewis index; TLI) is
#'    preferable for smaller samples. They should be > .90 (Byrne, 1994) or >
#'    .95 (Schumacker and Lomax, 2004).
#'
#'    \item **CFI**: The Comparative Fit Index is a revised form of NFI.
#'    Not very sensitive to sample size (Fan, Thompson, and Wang, 1999). Compares
#'    the fit of a target model to the fit of an independent, or null, model. It
#'    should be > .90.
#'
#'    \item **RMSEA**: The Root Mean Square Error of Approximation is a
#'    parsimony-adjusted index. Values closer to 0 represent a good fit. It
#'    should be < .08 or < .05. The p-value printed with it tests the hypothesis
#'    that RMSEA is less than or equal to .05 (a cutoff sometimes used for good
#'    fit), and thus should be not significant.
#'
#'    \item **RMR/SRMR**: the (Standardized) Root Mean Square Residual
#'    represents the square-root of the difference between the residuals of the
#'    sample covariance matrix and the hypothesized model. As the RMR can be
#'    sometimes hard to interpret, better to use SRMR. Should be < .08.
#'
#'    \item **RFI**: the Relative Fit Index, also known as RHO1, is not
#'    guaranteed to vary from 0 to 1. However, RFI close to 1 indicates a good
#'    fit.
#'
#'    \item **IFI**: the Incremental Fit Index (IFI) adjusts the Normed Fit
#'    Index (NFI) for sample size and degrees of freedom (Bollen's, 1989). Over
#'    0.90 is a good fit, but the index can exceed 1.
#'
#'    \item **PNFI**: the Parsimony-Adjusted Measures Index. There is no
#'    commonly agreed-upon cutoff value for an acceptable model for this index.
#'    Should be > 0.50. }
#'
#'    See the documentation for `?lavaan::fitmeasures`.
#' }
#'
#' \subsection{What to report}{
#' Kline (2015) suggests that at a minimum the following indices should be
#' reported: The model **chi-square**, the **RMSEA**, the **CFI**
#' and the **SRMR**.
#' }
#'
#' @examples
#' # Confirmatory Factor Analysis (CFA) ---------
#' if (require("lavaan")) {
#'   structure <- " visual  =~ x1 + x2 + x3
#'                  textual =~ x4 + x5 + x6
#'                  speed   =~ x7 + x8 + x9 "
#'   model <- lavaan::cfa(structure, data = HolzingerSwineford1939)
#'   model_performance(model)
#' }
#' @references
#'
#' - Byrne, B. M. (1994). Structural equation modeling with EQS and
#'   EQS/Windows. Thousand Oaks, CA: Sage Publications.
#'
#' - Tucker, L. R., and Lewis, C. (1973). The reliability coefficient for
#'   maximum likelihood factor analysis. Psychometrika, 38, 1-10.
#'
#' - Schumacker, R. E., and Lomax, R. G. (2004). A beginner's guide to
#'   structural equation modeling, Second edition. Mahwah, NJ: Lawrence Erlbaum
#'   Associates.
#'
#' - Fan, X., B. Thompson, and L. Wang (1999). Effects of sample size,
#'   estimation method, and model specification on structural equation modeling
#'   fit indexes. Structural Equation Modeling, 6, 56-83.
#'
#' - Kline, R. B. (2015). Principles and practice of structural equation
#'   modeling. Guilford publications.
#'
#' @export
model_performance.lavaan <- function(model, metrics = "all", verbose = TRUE, ...) {
  insight::check_if_installed("lavaan")


  if (isTRUE(verbose)) {
    measures <- as.data.frame(t(as.data.frame(lavaan::fitmeasures(model, ...))))
  } else {
    measures <- as.data.frame(t(as.data.frame(suppressWarnings(lavaan::fitmeasures(model, ...)))))
  }

  row.names(measures) <- NULL

  out <- data.frame(
    "Chi2" = measures$chisq,
    "Chi2_df" = measures$df,
    "p_Chi2" = measures$pvalue,
    "Baseline" = measures$baseline.chisq,
    "Baseline_df" = measures$baseline.df,
    "p_Baseline" = measures$baseline.pvalue,
    "GFI" = measures$gfi,
    "AGFI" = measures$agfi,
    "NFI" = measures$nfi,
    "NNFI" = measures$tli,
    "CFI" = measures$cfi,
    "RMSEA" = measures$rmsea,
    "RMSEA_CI_low" = measures$rmsea.ci.lower,
    "RMSEA_CI_high" = measures$rmsea.ci.upper,
    "p_RMSEA" = measures$rmsea.pvalue,
    "RMR" = measures$rmr,
    "SRMR" = measures$srmr,
    "RFI" = measures$rfi,
    "PNFI" = measures$pnfi,
    "IFI" = measures$ifi,
    "RNI" = measures$rni,
    "Loglikelihood" = measures$logl,
    "AIC" = measures$aic,
    "BIC" = measures$bic,
    "BIC_adjusted" = measures$bic2
  )

  if (all(metrics == "all")) {
    metrics <- names(out)
  }
  out <- out[, metrics]

  class(out) <- c("performance_lavaan", "performance_model", class(out))
  out
}



#' @export
model_performance.blavaan <- function(model, metrics = "all", verbose = TRUE, ...) {
  insight::check_if_installed(c("lavaan", "blavaan"))


  if (isTRUE(verbose)) {
    measures <- as.data.frame(t(as.data.frame(lavaan::fitmeasures(model, ...))))
    fitind <- summary(blavaan::blavFitIndices(model))
  } else {
    measures <- as.data.frame(t(as.data.frame(suppressWarnings(lavaan::fitmeasures(model, ...)))))
    fitind <- suppressWarnings(summary(blavaan::blavFitIndices(model)))
  }

  row.names(measures) <- NULL

  out <- data.frame(
    "BRMSEA" = fitind[1, "EAP"],
    "SD_BRMSEA" = fitind[1, "SD"],
    "BGammaHat" = fitind[2, "EAP"],
    "SD_BGammaHat" = fitind[2, "SD"],
    "Adj_BGammaHat" = fitind[3, "EAP"],
    "SD_Adj_BGammaHat" = fitind[3, "SD"],
    "Loglikelihood" = measures$logl,
    "BIC" = measures$bic,
    "DIC" = measures$dic,
    "p_DIC" = measures$p_dic,
    "WAIC" = measures$waic,
    "SE_WAIC" = measures$se_waic,
    "p_WAIC" = measures$p_waic,
    "LOOIC" = measures$looic,
    "SE_LOOIC" = measures$se_loo,
    "p_LOOIC" = measures$p_loo
  )

  if (all(metrics == "all")) {
    metrics <- names(out)
  }
  out <- out[, metrics]

  class(out) <- c("performance_lavaan", "performance_model", class(out))
  out
}
