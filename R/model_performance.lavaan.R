#' Performance of lavaan SEM / CFA Models
#'
#' Compute indices of model performance for SEM or CFA models from the
#' **lavaan** package.
#'
#' @param model A **lavaan** model, or a **lavaan.mi** model fitted to
#'   multiple imputed data sets.
#' @param metrics Can be `"all"` or a character vector of metrics to be
#'   computed (some of `"Chi2"`, `"Chi2_df"`, `"p_Chi2"`, `"Baseline"`,
#'   `"Baseline_df"`, `"p_Baseline"`, `"GFI"`, `"AGFI"`, `"NFI"`, `"NNFI"`,
#'   `"CFI"`, `"RMSEA"`, `"RMSEA_CI_low"`, `"RMSEA_CI_high"`, `"p_RMSEA"`,
#'   `"RMR"`, `"SRMR"`, `"RFI"`, `"PNFI"`, `"IFI"`, `"RNI"`, `"Loglikelihood"`,
#'   `"AIC"`, `"BIC"`, and `"BIC_adjusted"`.
#' @param verbose Toggle off warnings.
#' @param ... Arguments passed to or from other methods. For **lavaan** and
#'   **lavaan.mi** models, they are passed to `lavaan::fitMeasures()` (except
#'   `estimator`, which is ignored), for example `pool.method = "D2"` for
#'   **lavaan.mi** models.
#'
#' @return A data frame (with one row) and one column per "index" (see
#'   `metrics`). An index that `lavaan::fitMeasures()` does not return is `NA`.
#'
#' @section Models fitted to multiple imputed data:
#' For **lavaan.mi** models (for example, from `lavaan.mi::sem.mi()` or
#' `lavaan.mi::cfa.mi()`), the indices
#' come from the `fitMeasures()` method of the **lavaan.mi** package, which
#' pools the test statistics across imputations (by default with the `"D4"`
#' method). Pooling arguments, such as `pool.method`, pass through `...`. If no
#' imputation converged, all indices are `NA`. `compare_performance()` does not
#' accept **lavaan.mi** models yet, because **insight** does not recognize them
#' as supported models.
#'
#' @details \subsection{Indices of fit}{
#'
#'    - **Chisq**: The model Chi-squared assesses overall fit and the
#'    discrepancy between the sample and fitted covariance matrices. Its p-value
#'    should be > .05 (i.e., the hypothesis of a perfect fit cannot be
#'    rejected). However, it is quite sensitive to sample size.
#'
#'    - **GFI/AGFI**: The (Adjusted) Goodness of Fit is the proportion
#'    of variance accounted for by the estimated population covariance.
#'    Analogous to R2. The GFI and the AGFI should be > .95 and > .90,
#'    respectively.
#'
#'    - **NFI/NNFI/TLI**: The (Non) Normed Fit Index. An NFI of 0.95,
#'    indicates the model of interest improves the fit by 95\% relative to the
#'    null model. The NNFI (also called the Tucker Lewis index; TLI) is
#'    preferable for smaller samples. They should be > .90 (Byrne, 1994) or >
#'    .95 (Schumacker and Lomax, 2004).
#'
#'    - **CFI**: The Comparative Fit Index is a revised form of NFI.
#'    Not very sensitive to sample size (Fan, Thompson, and Wang, 1999). Compares
#'    the fit of a target model to the fit of an independent, or null, model. It
#'    should be > .90.
#'
#'    - **RMSEA**: The Root Mean Square Error of Approximation is a
#'    parsimony-adjusted index. Values closer to 0 represent a good fit. It
#'    should be < .08 or < .05. The p-value printed with it tests the hypothesis
#'    that RMSEA is less than or equal to .05 (a cutoff sometimes used for good
#'    fit), and thus should be not significant.
#'
#'    - **RMR/SRMR**: the (Standardized) Root Mean Square Residual
#'    represents the square-root of the difference between the residuals of the
#'    sample covariance matrix and the hypothesized model. As the RMR can be
#'    sometimes hard to interpret, better to use SRMR. Should be < .08.
#'
#'    - **RFI**: the Relative Fit Index, also known as RHO1, is not
#'    guaranteed to vary from 0 to 1. However, RFI close to 1 indicates a good
#'    fit.
#'
#'    - **IFI**: the Incremental Fit Index (IFI) adjusts the Normed Fit
#'    Index (NFI) for sample size and degrees of freedom (Bollen's, 1989). Over
#'    0.90 is a good fit, but the index can exceed 1.
#'
#'    - **PNFI**: the Parsimony-Adjusted Measures Index. There is no
#'    commonly agreed-upon cutoff value for an acceptable model for this index.
#'    Should be > 0.50. }
#'
#'    See the documentation for `?lavaan::fitmeasures`.
#'
#' \subsection{What to report}{
#' Kline (2015) suggests that at a minimum the following indices should be
#' reported: The model **chi-square**, the **RMSEA**, the **CFI**
#' and the **SRMR**.
#' }
#'
#' @examplesIf require("lavaan")
#' # Confirmatory Factor Analysis (CFA) ---------
#' data(HolzingerSwineford1939, package = "lavaan")
#' structure <- " visual  =~ x1 + x2 + x3
#'                textual =~ x4 + x5 + x6
#'                speed   =~ x7 + x8 + x9 "
#' model <- lavaan::cfa(structure, data = HolzingerSwineford1939)
#' model_performance(model)
#'
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
  .model_performance_lavaan(
    model,
    converged = insight::is_converged(model),
    metrics = metrics,
    verbose = verbose,
    not_converged = "This lavaan model did not converge, no performance indices can be computed. Returning `NA`.",
    ...
  )
}


#' @rdname model_performance.lavaan
#' @export
model_performance.lavaan.mi <- function(model, metrics = "all", verbose = TRUE, ...) {
  insight::check_if_installed(c("lavaan", "lavaan.mi"))
  # pooled fit measures need at least one converged imputation
  converged <- vapply(
    model@convergence,
    function(i) isTRUE(i[["converged"]]),
    logical(1)
  )
  .model_performance_lavaan(
    model,
    converged = any(converged),
    metrics = metrics,
    verbose = verbose,
    not_converged = "This lavaan.mi model did not converge in any imputation, no performance indices can be computed. Returning `NA`.",
    ...
  )
}


# helper ------------------------------------------------------------------

.model_performance_lavaan <- function(
  model,
  converged,
  metrics = "all",
  verbose = TRUE,
  not_converged = NULL,
  ...
) {
  # All possible metrics that this function can return.
  all_metric_names <- unname(.lavaan_fit_map())

  # extract valid column names based on metrics
  if (is.character(metrics) && !all(metrics == "all")) {
    out_names <- intersect(metrics, all_metric_names)
  } else {
    out_names <- all_metric_names
  }

  # process dots - we need to remove some arguments not supported in 'lavaan',
  # e.g. argument 'estimator' from "compare_performance()"
  dots <- list(...)
  dots$estimator <- NULL

  # create arguments for function call, with valid ellipses-dots
  fun_args <- c(list(model), dots)

  if (converged) {
    if (isTRUE(verbose)) {
      measures <- do.call(lavaan::fitmeasures, fun_args)
    } else {
      measures <- suppressWarnings(do.call(lavaan::fitmeasures, fun_args))
    }
    out <- .lavaan_fit_indices(measures)
  } else {
    if (verbose) {
      insight::format_warning(not_converged)
    }
    out <- as.data.frame(stats::setNames(
      rep(list(NA), length(out_names)),
      out_names
    ))
  }

  # select only the requested metrics
  out <- out[out_names]

  class(out) <- c("performance_lavaan", "performance_model", class(out))
  out
}


# entries of lavaan::fitmeasures(), named, and the columns they map to
.lavaan_fit_map <- function() {
  c(
    chisq = "Chi2",
    df = "Chi2_df",
    pvalue = "p_Chi2",
    baseline.chisq = "Baseline",
    baseline.df = "Baseline_df",
    baseline.pvalue = "p_Baseline",
    gfi = "GFI",
    agfi = "AGFI",
    nfi = "NFI",
    tli = "NNFI",
    cfi = "CFI",
    rmsea = "RMSEA",
    rmsea.ci.lower = "RMSEA_CI_low",
    rmsea.ci.upper = "RMSEA_CI_high",
    rmsea.pvalue = "p_RMSEA",
    rmr = "RMR",
    srmr = "SRMR",
    rfi = "RFI",
    pnfi = "PNFI",
    ifi = "IFI",
    rni = "RNI",
    logl = "Loglikelihood",
    aic = "AIC",
    bic = "BIC",
    bic2 = "BIC_adjusted"
  )
}


# one-row data frame of fit indices, `NA` for entries missing in `measures`
.lavaan_fit_indices <- function(measures) {
  fit_map <- .lavaan_fit_map()
  measures <- unclass(measures)
  # lavaan >= 0.7 (and lavaan.mi) return "agfi_lisrel" but no "agfi"
  if (!"agfi" %in% names(measures) && "agfi_lisrel" %in% names(measures)) {
    measures[["agfi"]] <- measures[["agfi_lisrel"]]
  }
  out <- lapply(names(fit_map), function(entry) {
    if (entry %in% names(measures)) {
      as.numeric(measures[[entry]])
    } else {
      NA_real_
    }
  })
  names(out) <- fit_map
  as.data.frame(out)
}


#' @export
model_performance.blavaan <- function(model, metrics = "all", verbose = TRUE, ...) {
  insight::check_if_installed(c("lavaan", "blavaan"))

  if (isTRUE(verbose)) {
    measures <- as.data.frame(t(as.data.frame(lavaan::fitmeasures(model, ...))))
    fitind <- summary(blavaan::blavFitIndices(model))
  } else {
    measures <- as.data.frame(t(as.data.frame(suppressWarnings(lavaan::fitmeasures(
      model,
      ...
    )))))
    fitind <- suppressWarnings(summary(blavaan::blavFitIndices(model)))
  }

  row.names(measures) <- NULL

  out <- data.frame(
    BRMSEA = fitind[1, "EAP"],
    SD_BRMSEA = fitind[1, "SD"],
    BGammaHat = fitind[2, "EAP"],
    SD_BGammaHat = fitind[2, "SD"],
    Adj_BGammaHat = fitind[3, "EAP"],
    SD_Adj_BGammaHat = fitind[3, "SD"],
    Loglikelihood = measures$logl,
    BIC = measures$bic,
    DIC = measures$dic,
    p_DIC = measures$p_dic,
    WAIC = measures$waic,
    SE_WAIC = measures$se_waic,
    p_WAIC = measures$p_waic,
    LOOIC = measures$looic,
    SE_LOOIC = measures$se_loo,
    p_LOOIC = measures$p_loo
  )

  if (all(metrics == "all")) {
    metrics <- names(out)
  }
  out <- out[, metrics, drop = FALSE]

  class(out) <- c("performance_lavaan", "performance_model", class(out))
  out
}
