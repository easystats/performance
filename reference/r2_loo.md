# LOO-adjusted R2

Compute LOO-adjusted R2.

## Usage

``` r
r2_loo(model, robust = TRUE, ci = 0.95, verbose = TRUE, ...)

r2_loo_posterior(model, ...)

# S3 method for class 'brmsfit'
r2_loo_posterior(model, verbose = TRUE, ...)

# S3 method for class 'stanreg'
r2_loo_posterior(model, verbose = TRUE, ...)
```

## Arguments

- model:

  A Bayesian regression model (from **brms**, **rstanarm**,
  **BayesFactor**, etc).

- robust:

  Logical, if `TRUE`, the median instead of mean is used to calculate
  the central tendency of the variances.

- ci:

  Value or vector of probability of the CI (between 0 and 1) to be
  estimated.

- verbose:

  Toggle off warnings.

- ...:

  Arguments passed to
  [`r2_posterior()`](https://easystats.github.io/performance/reference/r2_bayes.md).

## Value

A list with the Bayesian R2 value. For mixed models, a list with the
Bayesian R2 value and the marginal Bayesian R2 value. The standard
errors and credible intervals for the R2 values are saved as attributes.

A list with the LOO-adjusted R2 value. The standard errors and credible
intervals for the R2 values are saved as attributes.

## Details

`r2_loo()` returns an "adjusted" R2 value computed using a
leave-one-out-adjusted posterior distribution. This is conceptually
similar to an adjusted/unbiased R2 estimate in classical regression
modeling. See
[`r2_bayes()`](https://easystats.github.io/performance/reference/r2_bayes.md)
for an "unadjusted" R2.

Mixed models are not currently fully supported.

`r2_loo_posterior()` is the actual workhorse for `r2_loo()` and returns
a posterior sample of LOO-adjusted Bayesian R2 values.

## Examples

``` r
model <- suppressWarnings(rstanarm::stan_glm(
  mpg ~ wt + cyl,
  data = mtcars,
  chains = 1,
  iter = 500,
  refresh = 0,
  show_messages = FALSE
))
r2_loo(model)
#> Warning: Some Pareto k diagnostic values are too high. See help('pareto-k-diagnostic') for details.
#> # LOO-adjusted R2 with Compatibility Interval
#> 
#>   Conditional R2: 0.786 (95% CI [0.684, 0.880])
```
