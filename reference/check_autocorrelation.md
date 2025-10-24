# Check model for independence of residuals.

Check model for independence of residuals, i.e. for autocorrelation of
error terms.

## Usage

``` r
check_autocorrelation(x, ...)

# Default S3 method
check_autocorrelation(x, nsim = 1000, ...)

# S3 method for class 'performance_simres'
check_autocorrelation(x, time = NULL, ...)
```

## Arguments

- x:

  A model object, or an object returned by
  [`simulate_residuals()`](https://easystats.github.io/performance/reference/simulate_residuals.md).

- ...:

  Currently not used for models. For simulated residuals, arguments are
  passed to
  [`DHARMa::testTemporalAutocorrelation()`](https://rdrr.io/pkg/DHARMa/man/testTemporalAutocorrelation.html).

- nsim:

  Number of simulations for the Durbin-Watson-Test.

- time:

  A vector with time values to specify the temporal order of the data.
  Only used if `x` is an object returned by
  [`simulate_residuals()`](https://easystats.github.io/performance/reference/simulate_residuals.md)
  or by `DHARMa`.

## Value

Invisibly returns the p-value of the test statistics. A p-value \< 0.05
indicates autocorrelated residuals.

## Details

Performs a Durbin-Watson-Test to check for autocorrelated residuals. In
case of autocorrelation, robust standard errors return more accurate
results for the estimates, or maybe a mixed model with error term for
the cluster groups should be used.

## See also

Other functions to check model assumptions and and assess model quality:
[`check_collinearity()`](https://easystats.github.io/performance/reference/check_collinearity.md),
[`check_convergence()`](https://easystats.github.io/performance/reference/check_convergence.md),
[`check_heteroscedasticity()`](https://easystats.github.io/performance/reference/check_heteroscedasticity.md),
[`check_homogeneity()`](https://easystats.github.io/performance/reference/check_homogeneity.md),
[`check_model()`](https://easystats.github.io/performance/reference/check_model.md),
[`check_outliers()`](https://easystats.github.io/performance/reference/check_outliers.md),
[`check_overdispersion()`](https://easystats.github.io/performance/reference/check_overdispersion.md),
[`check_predictions()`](https://easystats.github.io/performance/reference/check_predictions.md),
[`check_singularity()`](https://easystats.github.io/performance/reference/check_singularity.md),
[`check_zeroinflation()`](https://easystats.github.io/performance/reference/check_zeroinflation.md)

## Examples

``` r
m <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
check_autocorrelation(m)
#> OK: Residuals appear to be independent and not autocorrelated (p = 0.338).
```
