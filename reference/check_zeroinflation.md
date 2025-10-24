# Check for zero-inflation in count models

`check_zeroinflation()` checks whether count models are over- or
underfitting zeros in the outcome.

## Usage

``` r
check_zeroinflation(x, ...)

# Default S3 method
check_zeroinflation(x, tolerance = 0.05, ...)

# S3 method for class 'performance_simres'
check_zeroinflation(x, tolerance = 0.1, alternative = "two.sided", ...)
```

## Arguments

- x:

  Fitted model of class `merMod`, `glmmTMB`, `glm`, or `glm.nb` (package
  **MASS**).

- ...:

  Arguments passed down to
  [`simulate_residuals()`](https://easystats.github.io/performance/reference/simulate_residuals.md).
  This only applies for models with zero-inflation component, or for
  models of class `glmmTMB` from `nbinom1` or `nbinom2` family.

- tolerance:

  The tolerance for the ratio of observed and predicted zeros to
  considered as over- or underfitting zeros. A ratio between 1 +/-
  `tolerance` is considered as OK, while a ratio beyond or below this
  threshold would indicate over- or underfitting.

- alternative:

  A character string specifying the alternative hypothesis. Can be one
  of `"two.sided"`, `"less"`, or `"greater"`.

## Value

A list with information about the amount of predicted and observed zeros
in the outcome, as well as the ratio between these two values.

## Details

If the amount of observed zeros is larger than the amount of predicted
zeros, the model is underfitting zeros, which indicates a zero-inflation
in the data. In such cases, it is recommended to use negative binomial
or zero-inflated models.

In case of negative binomial models, models with zero-inflation
component, or hurdle models, the results from `check_zeroinflation()`
are based on
[`simulate_residuals()`](https://easystats.github.io/performance/reference/simulate_residuals.md),
i.e. `check_zeroinflation(simulate_residuals(model))` is internally
called if necessary.

## Tests based on simulated residuals

For certain models, resp. model from certain families, tests are based
on simulated residuals (see
[`simulate_residuals()`](https://easystats.github.io/performance/reference/simulate_residuals.md)).
These are usually more accurate for testing such models than the
traditionally used Pearson residuals. However, when simulating from more
complex models, such as mixed models or models with zero-inflation,
there are several important considerations. Arguments specified in `...`
are passed to
[`simulate_residuals()`](https://easystats.github.io/performance/reference/simulate_residuals.md),
which relies on
[`DHARMa::simulateResiduals()`](https://rdrr.io/pkg/DHARMa/man/simulateResiduals.html)
(and therefore, arguments in `...` are passed further down to *DHARMa*).
The defaults in DHARMa are set on the most conservative option that
works for all models. However, in many cases, the help advises to use
different settings in particular situations or for particular models. It
is recommended to read the 'Details' in
[`?DHARMa::simulateResiduals`](https://rdrr.io/pkg/DHARMa/man/simulateResiduals.html)
closely to understand the implications of the simulation process and
which arguments should be modified to get the most accurate results.

## See also

Other functions to check model assumptions and and assess model quality:
[`check_autocorrelation()`](https://easystats.github.io/performance/reference/check_autocorrelation.md),
[`check_collinearity()`](https://easystats.github.io/performance/reference/check_collinearity.md),
[`check_convergence()`](https://easystats.github.io/performance/reference/check_convergence.md),
[`check_heteroscedasticity()`](https://easystats.github.io/performance/reference/check_heteroscedasticity.md),
[`check_homogeneity()`](https://easystats.github.io/performance/reference/check_homogeneity.md),
[`check_model()`](https://easystats.github.io/performance/reference/check_model.md),
[`check_outliers()`](https://easystats.github.io/performance/reference/check_outliers.md),
[`check_overdispersion()`](https://easystats.github.io/performance/reference/check_overdispersion.md),
[`check_predictions()`](https://easystats.github.io/performance/reference/check_predictions.md),
[`check_singularity()`](https://easystats.github.io/performance/reference/check_singularity.md)

## Examples

``` r
data(Salamanders, package = "glmmTMB")
m <- glm(count ~ spp + mined, family = poisson, data = Salamanders)
check_zeroinflation(m)
#> # Check for zero-inflation
#> 
#>    Observed zeros: 387
#>   Predicted zeros: 298
#>             Ratio: 0.77
#> 
#> Model is underfitting zeros (probable zero-inflation).

# for models with zero-inflation component, it's better to carry out
# the check for zero-inflation using simulated residuals
m <- glmmTMB::glmmTMB(
  count ~ spp + mined,
  ziformula = ~ mined + spp,
  family = poisson,
  data = Salamanders
)
res <- simulate_residuals(m)
check_zeroinflation(res)
#> # Check for zero-inflation
#> 
#>    Observed zeros: 387
#>   Predicted zeros: 387
#>             Ratio: 1.00
#> 
#> Model seems ok, ratio of observed and predicted zeros is within the
#>   tolerance range (p > .999).
```
