# Check overdispersion (and underdispersion) of GL(M)M's

`check_overdispersion()` checks generalized linear (mixed) models for
overdispersion (and underdispersion).

## Usage

``` r
check_overdispersion(x, ...)

# S3 method for class 'performance_simres'
check_overdispersion(x, alternative = "two.sided", ...)
```

## Arguments

- x:

  Fitted model of class `merMod`, `glmmTMB`, `glm`, or `glm.nb` (package
  **MASS**), or an object returned by
  [`simulate_residuals()`](https://easystats.github.io/performance/reference/simulate_residuals.md).

- ...:

  Arguments passed down to
  [`simulate_residuals()`](https://easystats.github.io/performance/reference/simulate_residuals.md).
  This only applies for models with zero-inflation component, or for
  models of class `glmmTMB` from `nbinom1` or `nbinom2` family.

- alternative:

  A character string specifying the alternative hypothesis. Can be one
  of `"two.sided"`, `"less"`, or `"greater"`.

## Value

A list with results from the overdispersion test, like chi-squared
statistics, p-value or dispersion ratio.

## Details

Overdispersion occurs when the observed variance is higher than the
variance of a theoretical model. For Poisson models, variance increases
with the mean and, therefore, variance usually (roughly) equals the mean
value. If the variance is much higher, the data are "overdispersed". A
less common case is underdispersion, where the variance is much lower
than the mean.

## Interpretation of the Dispersion Ratio

If the dispersion ratio is close to one, a Poisson model fits well to
the data. Dispersion ratios larger than one indicate overdispersion,
thus a negative binomial model or similar might fit better to the data.
Dispersion ratios much smaller than one indicate underdispersion. A
p-value \< .05 indicates either overdispersion or underdispersion (the
first being more common).

## Overdispersion in Poisson Models

For Poisson models, the overdispersion test is based on the code from
*Gelman and Hill (2007), page 115*.

## Overdispersion in Negative Binomial or Zero-Inflated Models

For negative binomial (mixed) models or models with zero-inflation
component, the overdispersion test is based simulated residuals (see
[`simulate_residuals()`](https://easystats.github.io/performance/reference/simulate_residuals.md)).

## Overdispersion in Mixed Models

For `merMod`- and `glmmTMB`-objects, `check_overdispersion()` is based
on the code in the [GLMM
FAQ](http://bbolker.github.io/mixedmodels-misc/glmmFAQ.md), section *How
can I deal with overdispersion in GLMMs?*. Note that this function only
returns an *approximate* estimate of an overdispersion parameter. Using
this approach would be inaccurate for zero-inflated or negative binomial
mixed models (fitted with `glmmTMB`), thus, in such cases, the
overdispersion test is based on
[`simulate_residuals()`](https://easystats.github.io/performance/reference/simulate_residuals.md)
(which is identical to
`check_overdispersion(simulate_residuals(model))`).

## How to fix Overdispersion

Overdispersion can be fixed by either modeling the dispersion parameter,
or by choosing a different distributional family (like Quasi-Poisson, or
negative binomial, see *Gelman and Hill (2007), pages 115-116*).

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

## References

- Bolker B et al. (2017): [GLMM
  FAQ.](http://bbolker.github.io/mixedmodels-misc/glmmFAQ.md)

- Gelman, A., and Hill, J. (2007). Data analysis using regression and
  multilevel/hierarchical models. Cambridge; New York: Cambridge
  University Press.

## See also

Other functions to check model assumptions and and assess model quality:
[`check_autocorrelation()`](https://easystats.github.io/performance/reference/check_autocorrelation.md),
[`check_collinearity()`](https://easystats.github.io/performance/reference/check_collinearity.md),
[`check_convergence()`](https://easystats.github.io/performance/reference/check_convergence.md),
[`check_heteroscedasticity()`](https://easystats.github.io/performance/reference/check_heteroscedasticity.md),
[`check_homogeneity()`](https://easystats.github.io/performance/reference/check_homogeneity.md),
[`check_model()`](https://easystats.github.io/performance/reference/check_model.md),
[`check_outliers()`](https://easystats.github.io/performance/reference/check_outliers.md),
[`check_predictions()`](https://easystats.github.io/performance/reference/check_predictions.md),
[`check_singularity()`](https://easystats.github.io/performance/reference/check_singularity.md),
[`check_zeroinflation()`](https://easystats.github.io/performance/reference/check_zeroinflation.md)

## Examples

``` r
data(Salamanders, package = "glmmTMB")
m <- glm(count ~ spp + mined, family = poisson, data = Salamanders)
check_overdispersion(m)
#> # Overdispersion test
#> 
#>        dispersion ratio =    2.946
#>   Pearson's Chi-Squared = 1873.710
#>                 p-value =  < 0.001
#> 
#> Overdispersion detected.
```
