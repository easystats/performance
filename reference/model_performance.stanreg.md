# Performance of Bayesian Models

Compute indices of model performance for (general) linear models.

## Usage

``` r
# S3 method for class 'stanreg'
model_performance(model, metrics = "all", verbose = TRUE, ...)

# S3 method for class 'BFBayesFactor'
model_performance(
  model,
  metrics = "all",
  verbose = TRUE,
  average = FALSE,
  prior_odds = NULL,
  ...
)
```

## Arguments

- model:

  Object of class `stanreg` or `brmsfit`.

- metrics:

  Can be `"all"`, `"common"` or a character vector of metrics to be
  computed (some of
  `c("LOOIC", "WAIC", "R2", "R2_adj", "RMSE", "SIGMA", "LOGLOSS", "SCORE")`).
  `"common"` will compute LOOIC, WAIC, R2 and RMSE.

- verbose:

  Toggle off warnings.

- ...:

  Arguments passed to or from other methods.

- average:

  Compute model-averaged index? See
  [`bayestestR::weighted_posteriors()`](https://easystats.github.io/bayestestR/reference/weighted_posteriors.html).

- prior_odds:

  Optional vector of prior odds for the models compared to the first
  model (or the denominator, for `BFBayesFactor` objects). For
  `data.frame`s, this will be used as the basis of weighting.

## Value

A data frame (with one row) and one column per "index" (see `metrics`).

## Details

Depending on `model`, the following indices are computed:

- **ELPD**: expected log predictive density. Larger ELPD values mean
  better fit. See
  [`looic()`](https://easystats.github.io/performance/reference/looic.md).

- **LOOIC**: leave-one-out cross-validation (LOO) information criterion.
  Lower LOOIC values mean better fit. See
  [`looic()`](https://easystats.github.io/performance/reference/looic.md).

- **WAIC**: widely applicable information criterion. Lower WAIC values
  mean better fit. See
  [`?loo::waic`](https://mc-stan.org/loo/reference/waic.html).

- **R2**: r-squared value, see
  [`r2_bayes()`](https://easystats.github.io/performance/reference/r2_bayes.md).

- **R2_adjusted**: LOO-adjusted r-squared, see
  [`r2_loo()`](https://easystats.github.io/performance/reference/r2_loo.md).

- **RMSE**: root mean squared error, see
  [`performance_rmse()`](https://easystats.github.io/performance/reference/performance_rmse.md).

- **SIGMA**: residual standard deviation, see
  [`insight::get_sigma()`](https://easystats.github.io/insight/reference/get_sigma.html).

- **LOGLOSS**: Log-loss, see
  [`performance_logloss()`](https://easystats.github.io/performance/reference/performance_logloss.md).

- **SCORE_LOG**: score of logarithmic proper scoring rule, see
  [`performance_score()`](https://easystats.github.io/performance/reference/performance_score.md).

- **SCORE_SPHERICAL**: score of spherical proper scoring rule, see
  [`performance_score()`](https://easystats.github.io/performance/reference/performance_score.md).

- **PCP**: percentage of correct predictions, see
  [`performance_pcp()`](https://easystats.github.io/performance/reference/performance_pcp.md).

## References

Gelman, A., Goodrich, B., Gabry, J., and Vehtari, A. (2018). R-squared
for Bayesian regression models. The American Statistician, The American
Statistician, 1-6.

## See also

[r2_bayes](https://easystats.github.io/performance/reference/r2_bayes.md)

## Examples

``` r
# \donttest{
model <- suppressWarnings(rstanarm::stan_glm(
  mpg ~ wt + cyl,
  data = mtcars,
  chains = 1,
  iter = 500,
  refresh = 0
))
model_performance(model)
#> # Indices of model performance
#> 
#> ELPD    | ELPD_SE | LOOIC | LOOIC_SE |  WAIC |    R2 | R2 (adj.) |  RMSE | Sigma
#> --------------------------------------------------------------------------------
#> -78.013 |   4.461 | 156.0 |    8.921 | 156.0 | 0.815 |     0.801 | 2.464 | 2.602

model <- suppressWarnings(rstanarm::stan_glmer(
  mpg ~ wt + cyl + (1 | gear),
  data = mtcars,
  chains = 1,
  iter = 500,
  refresh = 0
))
model_performance(model)
#> # Indices of model performance
#> 
#> ELPD    | ELPD_SE | LOOIC | LOOIC_SE |  WAIC |    R2 | R2 (marg.) | R2 (adj.)
#> -----------------------------------------------------------------------------
#> -79.115 |   4.724 | 158.2 |    9.447 | 158.2 | 0.823 |      0.826 |     0.794
#> 
#> ELPD    | R2_adjusted_marginal |   ICC |  RMSE | Sigma
#> ------------------------------------------------------
#> -79.115 |                0.794 | 0.234 | 2.438 | 2.597
# }
```
