# Performance of instrumental variable regression models

Performance of instrumental variable regression models

## Usage

``` r
# S3 method for class 'ivreg'
model_performance(model, metrics = "all", verbose = TRUE, ...)
```

## Arguments

- model:

  A model.

- metrics:

  Can be `"all"`, `"common"` or a character vector of metrics to be
  computed (some of
  `c("AIC", "AICc", "BIC", "R2", "RMSE", "SIGMA", "Sargan", "Wu_Hausman", "weak_instruments")`).
  `"common"` will compute AIC, BIC, R2 and RMSE.

- verbose:

  Toggle off warnings.

- ...:

  Arguments passed to or from other methods.

## Details

[`model_performance()`](https://easystats.github.io/performance/reference/model_performance.md)
correctly detects transformed response and returns the "corrected" AIC
and BIC value on the original scale. To get back to the original scale,
the likelihood of the model is multiplied by the Jacobian/derivative of
the transformation.
