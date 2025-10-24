# Performance of lavaan SEM / CFA Models

Compute indices of model performance for SEM or CFA models from the
**lavaan** package.

## Usage

``` r
# S3 method for class 'lavaan'
model_performance(model, metrics = "all", verbose = TRUE, ...)
```

## Arguments

- model:

  A **lavaan** model.

- metrics:

  Can be `"all"` or a character vector of metrics to be computed (some
  of `"Chi2"`, `"Chi2_df"`, `"p_Chi2"`, `"Baseline"`, `"Baseline_df"`,
  `"p_Baseline"`, `"GFI"`, `"AGFI"`, `"NFI"`, `"NNFI"`, `"CFI"`,
  `"RMSEA"`, `"RMSEA_CI_low"`, `"RMSEA_CI_high"`, `"p_RMSEA"`, `"RMR"`,
  `"SRMR"`, `"RFI"`, `"PNFI"`, `"IFI"`, `"RNI"`, `"Loglikelihood"`,
  `"AIC"`, `"BIC"`, and `"BIC_adjusted"`.

- verbose:

  Toggle off warnings.

- ...:

  Arguments passed to or from other methods.

## Value

A data frame (with one row) and one column per "index" (see `metrics`).

## Details

### Indices of fit

- **Chisq**: The model Chi-squared assesses overall fit and the
  discrepancy between the sample and fitted covariance matrices. Its
  p-value should be \> .05 (i.e., the hypothesis of a perfect fit cannot
  be rejected). However, it is quite sensitive to sample size.

- **GFI/AGFI**: The (Adjusted) Goodness of Fit is the proportion of
  variance accounted for by the estimated population covariance.
  Analogous to R2. The GFI and the AGFI should be \> .95 and \> .90,
  respectively.

- **NFI/NNFI/TLI**: The (Non) Normed Fit Index. An NFI of 0.95,
  indicates the model of interest improves the fit by 95\\ null model.
  The NNFI (also called the Tucker Lewis index; TLI) is preferable for
  smaller samples. They should be \> .90 (Byrne, 1994) or \> .95
  (Schumacker and Lomax, 2004).

- **CFI**: The Comparative Fit Index is a revised form of NFI. Not very
  sensitive to sample size (Fan, Thompson, and Wang, 1999). Compares the
  fit of a target model to the fit of an independent, or null, model. It
  should be \> .90.

- **RMSEA**: The Root Mean Square Error of Approximation is a
  parsimony-adjusted index. Values closer to 0 represent a good fit. It
  should be \< .08 or \< .05. The p-value printed with it tests the
  hypothesis that RMSEA is less than or equal to .05 (a cutoff sometimes
  used for good fit), and thus should be not significant.

- **RMR/SRMR**: the (Standardized) Root Mean Square Residual represents
  the square-root of the difference between the residuals of the sample
  covariance matrix and the hypothesized model. As the RMR can be
  sometimes hard to interpret, better to use SRMR. Should be \< .08.

- **RFI**: the Relative Fit Index, also known as RHO1, is not guaranteed
  to vary from 0 to 1. However, RFI close to 1 indicates a good fit.

- **IFI**: the Incremental Fit Index (IFI) adjusts the Normed Fit Index
  (NFI) for sample size and degrees of freedom (Bollen's, 1989). Over
  0.90 is a good fit, but the index can exceed 1.

- **PNFI**: the Parsimony-Adjusted Measures Index. There is no commonly
  agreed-upon cutoff value for an acceptable model for this index.
  Should be \> 0.50.

See the documentation for
[`?lavaan::fitmeasures`](https://rdrr.io/pkg/lavaan/man/fitMeasures.html).

### What to report

Kline (2015) suggests that at a minimum the following indices should be
reported: The model **chi-square**, the **RMSEA**, the **CFI** and the
**SRMR**.

## References

- Byrne, B. M. (1994). Structural equation modeling with EQS and
  EQS/Windows. Thousand Oaks, CA: Sage Publications.

- Tucker, L. R., and Lewis, C. (1973). The reliability coefficient for
  maximum likelihood factor analysis. Psychometrika, 38, 1-10.

- Schumacker, R. E., and Lomax, R. G. (2004). A beginner's guide to
  structural equation modeling, Second edition. Mahwah, NJ: Lawrence
  Erlbaum Associates.

- Fan, X., B. Thompson, and L. Wang (1999). Effects of sample size,
  estimation method, and model specification on structural equation
  modeling fit indexes. Structural Equation Modeling, 6, 56-83.

- Kline, R. B. (2015). Principles and practice of structural equation
  modeling. Guilford publications.

## Examples

``` r
# Confirmatory Factor Analysis (CFA) ---------
data(HolzingerSwineford1939, package = "lavaan")
structure <- " visual  =~ x1 + x2 + x3
               textual =~ x4 + x5 + x6
               speed   =~ x7 + x8 + x9 "
model <- lavaan::cfa(structure, data = HolzingerSwineford1939)
model_performance(model)
#> # Indices of model performance
#> 
#> Chi2(24) | p (Chi2) | Baseline(36) | p (Baseline) |   GFI |  AGFI |   NFI
#> -------------------------------------------------------------------------
#> 85.306   |   < .001 |      918.852 |       < .001 | 0.943 | 0.894 | 0.907
#> 
#> Chi2(24) |  NNFI |   CFI | RMSEA |      RMSEA  CI | p (RMSEA) |   RMR |  SRMR
#> -----------------------------------------------------------------------------
#> 85.306   | 0.896 | 0.931 | 0.092 | [0.071, 0.114] |    < .001 | 0.082 | 0.065
#> 
#> Chi2(24) |   RFI |  PNFI |   IFI |   RNI | Loglikelihood |    AIC |    BIC | BIC_adjusted
#> -----------------------------------------------------------------------------------------
#> 85.306   | 0.861 | 0.605 | 0.931 | 0.931 |     -3737.745 | 7517.5 | 7595.3 |     7528.739
```
