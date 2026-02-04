# Compare performance of different models

`compare_performance()` computes indices of model performance for
different models at once and hence allows comparison of indices across
models.

## Usage

``` r
compare_performance(
  ...,
  metrics = "all",
  rank = FALSE,
  estimator = "ML",
  verbose = TRUE
)
```

## Arguments

- ...:

  Multiple model objects (also of different classes).

- metrics:

  Can be `"all"`, `"common"` or a character vector of metrics to be
  computed. See related
  [`documentation()`](https://easystats.github.io/performance/reference/model_performance.md)
  of object's class for details.

- rank:

  Logical, if `TRUE`, models are ranked according to 'best' overall
  model performance. See 'Details'.

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

  Toggle warnings.

## Value

A data frame with one row per model and one column per "index" (see
`metrics`).

## Details

### Model Weights

When information criteria (IC) are requested in `metrics` (i.e., any of
`"all"`, `"common"`, `"AIC"`, `"AICc"`, `"BIC"`, `"WAIC"`, or
`"LOOIC"`), model weights based on these criteria are also computed. For
all IC except LOOIC, weights are computed as
`w = exp(-0.5 * delta_ic) / sum(exp(-0.5 * delta_ic))`, where `delta_ic`
is the difference between the model's IC value and the smallest IC value
in the model set (Burnham and Anderson, 2002). For LOOIC, weights are
computed as "stacking weights" using
[`loo::stacking_weights()`](https://mc-stan.org/loo/reference/loo_model_weights.html).

### Ranking Models

When `rank = TRUE`, a new column `Performance_Score` is returned. This
score ranges from 0\\ performance. Note that all score value do not
necessarily sum up to 100\\ Rather, calculation is based on normalizing
all indices (i.e. rescaling them to a range from 0 to 1), and taking the
mean value of all indices for each model. This is a rather quick
heuristic, but might be helpful as exploratory index.\
\
In particular when models are of different types (e.g. mixed models,
classical linear models, logistic regression, ...), not all indices will
be computed for each model. In case where an index can't be calculated
for a specific model type, this model gets an `NA` value. All indices
that have any `NA`s are excluded from calculating the performance
score.\
\
There is a
[`plot()`](https://rdrr.io/r/graphics/plot.default.html)-method for
`compare_performance()`, which creates a "spiderweb" plot, where the
different indices are normalized and larger values indicate better model
performance. Hence, points closer to the center indicate worse fit
indices (see
[online-documentation](https://easystats.github.io/see/articles/performance.html)
for more details).

### REML versus ML estimator

By default, `estimator = "ML"`, which means that values from information
criteria (AIC, AICc, BIC) for specific model classes (like models from
*lme4*) are based on the ML-estimator, while the default behaviour of
[`AIC()`](https://rdrr.io/r/stats/AIC.html) for such classes is setting
`REML = TRUE`. This default is intentional, because comparing
information criteria based on REML fits is usually not valid (it might
be useful, though, if all models share the same fixed effects - however,
this is usually not the case for nested models, which is a prerequisite
for the LRT). Set `estimator = "REML"` explicitly return the same
(AIC/...) values as from the defaults in `AIC.merMod()`.

## Note

There is also a
[[`plot()`](https://rdrr.io/r/graphics/plot.default.html)-method](https://easystats.github.io/see/articles/performance.html)
implemented in the [see-package](https://easystats.github.io/see/).

## References

Burnham, K. P., and Anderson, D. R. (2002). *Model selection and
multimodel inference: A practical information-theoretic approach* (2nd
ed.). Springer-Verlag.
[doi:10.1007/b97636](https://doi.org/10.1007/b97636)

## Examples

``` r
data(iris)
lm1 <- lm(Sepal.Length ~ Species, data = iris)
lm2 <- lm(Sepal.Length ~ Species + Petal.Length, data = iris)
lm3 <- lm(Sepal.Length ~ Species * Petal.Length, data = iris)
compare_performance(lm1, lm2, lm3)
#> # Comparison of Model Performance Indices
#> 
#> Name | Model | AIC (weights) | AICc (weights) | BIC (weights) |    R2
#> ---------------------------------------------------------------------
#> lm1  |    lm | 231.5 (<.001) |  231.7 (<.001) | 243.5 (<.001) | 0.619
#> lm2  |    lm | 106.2 (0.566) |  106.6 (0.611) | 121.3 (0.964) | 0.837
#> lm3  |    lm | 106.8 (0.434) |  107.6 (0.389) | 127.8 (0.036) | 0.840
#> 
#> Name | R2 (adj.) |  RMSE | Sigma
#> --------------------------------
#> lm1  |     0.614 | 0.510 | 0.515
#> lm2  |     0.833 | 0.333 | 0.338
#> lm3  |     0.835 | 0.330 | 0.336
compare_performance(lm1, lm2, lm3, rank = TRUE)
#> # Comparison of Model Performance Indices
#> 
#> Name | Model |    R2 | R2 (adj.) |  RMSE | Sigma | AIC weights | AICc weights
#> -----------------------------------------------------------------------------
#> lm2  |    lm | 0.837 |     0.833 | 0.333 | 0.338 |       0.566 |        0.611
#> lm3  |    lm | 0.840 |     0.835 | 0.330 | 0.336 |       0.434 |        0.389
#> lm1  |    lm | 0.619 |     0.614 | 0.510 | 0.515 |    3.65e-28 |     4.23e-28
#> 
#> Name | BIC weights | Performance-Score
#> --------------------------------------
#> lm2  |       0.964 |            99.23%
#> lm3  |       0.036 |            77.70%
#> lm1  |    2.80e-27 |             0.00%

m1 <- lm(mpg ~ wt + cyl, data = mtcars)
m2 <- glm(vs ~ wt + mpg, data = mtcars, family = "binomial")
m3 <- lme4::lmer(Petal.Length ~ Sepal.Length + (1 | Species), data = iris)
compare_performance(m1, m2, m3)
#> When comparing models, please note that probably not all models were fit
#>   from same data.
#> # Comparison of Model Performance Indices
#> 
#> Name |   Model | AIC (weights) | AICc (weights) | BIC (weights) |  RMSE | Sigma
#> -------------------------------------------------------------------------------
#> m1   |      lm | 156.0 (<.001) |  157.5 (<.001) | 161.9 (<.001) | 2.444 | 2.568
#> m2   |     glm |  31.3 (>.999) |   32.2 (>.999) |  35.7 (>.999) | 0.359 | 1.000
#> m3   | lmerMod |  74.6 (<.001) |   74.9 (<.001) |  86.7 (<.001) | 0.279 | 0.283
#> 
#> Name |    R2 | R2 (adj.) | Tjur's R2 | Log_loss | Score_log | Score_spherical
#> -----------------------------------------------------------------------------
#> m1   | 0.830 |     0.819 |           |          |           |                
#> m2   |       |           |     0.478 |    0.395 |   -14.903 |           0.095
#> m3   |       |           |           |          |           |                
#> 
#> Name |   PCP | R2 (cond.) | R2 (marg.) |   ICC
#> ----------------------------------------------
#> m1   |       |            |            |      
#> m2   | 0.743 |            |            |      
#> m3   |       |      0.972 |      0.096 | 0.969
```
