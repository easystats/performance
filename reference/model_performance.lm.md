# Performance of Regression Models

Compute indices of model performance for regression models.

## Usage

``` r
# S3 method for class 'lm'
model_performance(model, metrics = "all", verbose = TRUE, ...)
```

## Arguments

- model:

  A model.

- metrics:

  Can be `"all"`, `"common"` or a character vector of metrics to be
  computed (one or more of `"AIC"`, `"AICc"`, `"BIC"`, `"R2"`,
  `"R2_adj"`, `"RMSE"`, `"SIGMA"`, `"LOGLOSS"`, `"PCP"`, `"SCORE"`).
  `"common"` will compute AIC, BIC, R2 and RMSE.

- verbose:

  Toggle off warnings.

- ...:

  Arguments passed to or from other methods.

## Value

A data frame (with one row) and one column per "index" (see `metrics`).

## Details

Depending on `model`, following indices are computed:

- **AIC**: Akaike's Information Criterion, see
  [`?stats::AIC`](https://rdrr.io/r/stats/AIC.html)

- **AICc**: Second-order (or small sample) AIC with a correction for
  small sample sizes

- **BIC**: Bayesian Information Criterion, see
  [`?stats::BIC`](https://rdrr.io/r/stats/AIC.html)

- **R2**: r-squared value, see
  [`r2()`](https://easystats.github.io/performance/reference/r2.md)

- **R2_adj**: adjusted r-squared, see
  [`r2()`](https://easystats.github.io/performance/reference/r2.md)

- **RMSE**: root mean squared error, see
  [`performance_rmse()`](https://easystats.github.io/performance/reference/performance_rmse.md)

- **SIGMA**: residual standard deviation, see
  [`insight::get_sigma()`](https://easystats.github.io/insight/reference/get_sigma.html)

- **LOGLOSS**: Log-loss, see
  [`performance_logloss()`](https://easystats.github.io/performance/reference/performance_logloss.md)

- **SCORE_LOG**: score of logarithmic proper scoring rule, see
  [`performance_score()`](https://easystats.github.io/performance/reference/performance_score.md)

- **SCORE_SPHERICAL**: score of spherical proper scoring rule, see
  [`performance_score()`](https://easystats.github.io/performance/reference/performance_score.md)

- **PCP**: percentage of correct predictions, see
  [`performance_pcp()`](https://easystats.github.io/performance/reference/performance_pcp.md)

[`model_performance()`](https://easystats.github.io/performance/reference/model_performance.md)
correctly detects transformed response and returns the "corrected" AIC
and BIC value on the original scale. To get back to the original scale,
the likelihood of the model is multiplied by the Jacobian/derivative of
the transformation.

## Examples

``` r
model <- lm(mpg ~ wt + cyl, data = mtcars)
model_performance(model)
#> # Indices of model performance
#> 
#> AIC   |  AICc |   BIC |    R2 | R2 (adj.) |  RMSE | Sigma
#> ---------------------------------------------------------
#> 156.0 | 157.5 | 161.9 | 0.830 |     0.819 | 2.444 | 2.568

model <- glm(vs ~ wt + mpg, data = mtcars, family = "binomial")
model_performance(model)
#> # Indices of model performance
#> 
#> AIC  | AICc |  BIC | Tjur's R2 |  RMSE | Sigma | Log_loss | Score_log
#> ---------------------------------------------------------------------
#> 31.3 | 32.2 | 35.7 |     0.478 | 0.359 |     1 |    0.395 |   -14.903
#> 
#> AIC  | Score_spherical |   PCP
#> ------------------------------
#> 31.3 |           0.095 | 0.743
```
