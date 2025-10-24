# Performance of Mixed Models

Compute indices of model performance for mixed models.

## Usage

``` r
# S3 method for class 'merMod'
model_performance(
  model,
  metrics = "all",
  estimator = "REML",
  verbose = TRUE,
  ...
)
```

## Arguments

- model:

  A mixed effects model.

- metrics:

  Can be `"all"`, `"common"` or a character vector of metrics to be
  computed (some of
  `c("AIC", "AICc", "BIC", "R2", "ICC", "RMSE", "SIGMA", "LOGLOSS", "SCORE")`).
  `"common"` will compute AIC, BIC, R2, ICC and RMSE.

- estimator:

  Only for linear models. Corresponds to the different estimators for
  the standard deviation of the errors. If `estimator = "ML"` (default,
  except for
  [`performance_aic()`](https://easystats.github.io/performance/reference/performance_aicc.md)
  when the model object is of class `lmerMod`), the scaling is done by
  `n` (the biased ML estimator), which is then equivalent to using
  `AIC(logLik())`. Setting it to `"REML"` will give the same results as
  `AIC(logLik(..., REML = TRUE))`.

- verbose:

  Toggle warnings and messages.

- ...:

  Arguments passed to or from other methods.

## Value

A data frame (with one row) and one column per "index" (see `metrics`).

## Details

### Intraclass Correlation Coefficient (ICC)

This method returns the *adjusted ICC* only, as this is typically of
interest when judging the variance attributed to the random effects part
of the model (see also
[`icc()`](https://easystats.github.io/performance/reference/icc.md)).

### REML versus ML estimator

The default behaviour of
[`model_performance()`](https://easystats.github.io/performance/reference/model_performance.md)
when computing AIC or BIC of linear mixed model from package **lme4** is
the same as for [`AIC()`](https://rdrr.io/r/stats/AIC.html) or
[`BIC()`](https://rdrr.io/r/stats/AIC.html) (i.e. `estimator = "REML"`).
However, for model comparison using
[`compare_performance()`](https://easystats.github.io/performance/reference/compare_performance.md)
sets `estimator = "ML"` by default, because *comparing* information
criteria based on REML fits is usually not valid (unless all models have
the same fixed effects). Thus, make sure to set the correct
estimator-value when looking at fit-indices or comparing model fits.

### Other performance indices

Furthermore, see 'Details' in
[`model_performance.lm()`](https://easystats.github.io/performance/reference/model_performance.lm.md)
for more details on returned indices.

## Examples

``` r
model <- lme4::lmer(Petal.Length ~ Sepal.Length + (1 | Species), data = iris)
model_performance(model)
#> # Indices of model performance
#> 
#> AIC  | AICc |  BIC | R2 (cond.) | R2 (marg.) |   ICC |  RMSE | Sigma
#> --------------------------------------------------------------------
#> 77.3 | 77.6 | 89.4 |      0.972 |      0.096 | 0.969 | 0.279 | 0.283
```
