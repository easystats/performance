# Model Performance

See the documentation for your object's class:

- [Frequentist
  Regressions](https://easystats.github.io/performance/reference/model_performance.lm.md)

- [Instrumental Variables
  Regressions](https://easystats.github.io/performance/reference/model_performance.ivreg.md)

- [Mixed
  models](https://easystats.github.io/performance/reference/model_performance.merMod.md)

- [Bayesian
  models](https://easystats.github.io/performance/reference/model_performance.stanreg.md)

- [CFA / SEM lavaan
  models](https://easystats.github.io/performance/reference/model_performance.lavaan.md)

- [Meta-analysis
  models](https://easystats.github.io/performance/reference/model_performance.rma.md)

## Usage

``` r
model_performance(model, ...)

performance(model, ...)
```

## Arguments

- model:

  Statistical model.

- ...:

  Arguments passed to or from other methods, resp. for
  [`compare_performance()`](https://easystats.github.io/performance/reference/compare_performance.md),
  one or multiple model objects (also of different classes).

## Value

A data frame (with one row) and one column per "index" (see `metrics`).

## Details

`model_performance()` correctly detects transformed response and returns
the "corrected" AIC and BIC value on the original scale. To get back to
the original scale, the likelihood of the model is multiplied by the
Jacobian/derivative of the transformation.

## See also

[`compare_performance()`](https://easystats.github.io/performance/reference/compare_performance.md)
to compare performance of many different models.

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
