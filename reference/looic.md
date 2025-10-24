# LOO-related Indices for Bayesian regressions.

Compute LOOIC (leave-one-out cross-validation (LOO) information
criterion) and ELPD (expected log predictive density) for Bayesian
regressions. For LOOIC and ELPD, smaller and larger values are
respectively indicative of a better fit.

## Usage

``` r
looic(model, verbose = TRUE)
```

## Arguments

- model:

  A Bayesian regression model.

- verbose:

  Toggle off warnings.

## Value

A list with four elements, the ELPD, LOOIC and their standard errors.

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
looic(model)
#> # LOOIC and ELPD with Standard Error
#> 
#>   LOOIC: 157.05 [9.03]
#>    ELPD: -78.52 [4.51]
# }
```
