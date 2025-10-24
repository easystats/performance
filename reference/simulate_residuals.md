# Simulate randomized quantile residuals from a model

Returns simulated residuals from a model. This is useful for checking
the uniformity of residuals, in particular for non-Gaussian models,
where the residuals are not expected to be normally distributed.

## Usage

``` r
simulate_residuals(x, iterations = 250, ...)

# S3 method for class 'performance_simres'
residuals(object, quantile_function = NULL, outlier_values = NULL, ...)
```

## Arguments

- x:

  A model object.

- iterations:

  Number of simulations to run.

- ...:

  Arguments passed on to
  [`DHARMa::simulateResiduals()`](https://rdrr.io/pkg/DHARMa/man/simulateResiduals.html).

- object:

  A `performance_simres` object, as returned by `simulate_residuals()`.

- quantile_function:

  A function to apply to the residuals. If `NULL`, the residuals are
  returned as is. If not `NULL`, the residuals are passed to this
  function. This is useful for returning normally distributed residuals,
  for example: `residuals(x, quantile_function = qnorm)`.

- outlier_values:

  A vector of length 2, specifying the values to replace `-Inf` and
  `Inf` with, respectively.

## Value

Simulated residuals, which can be further processed with
[`check_residuals()`](https://easystats.github.io/performance/reference/check_residuals.md).
The returned object is of class `DHARMa` and `performance_simres`.

## Details

This function is a small wrapper around
[`DHARMa::simulateResiduals()`](https://rdrr.io/pkg/DHARMa/man/simulateResiduals.html).
It basically only sets `plot = FALSE` and adds an additional class
attribute (`"performance_sim_res"`), which allows using the DHARMa
object in own plotting functions from the **see** package. See also
`vignette("DHARMa")`. There is a
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) method to
visualize the distribution of the residuals.

## Tests based on simulated residuals

For certain models, resp. model from certain families, tests like
[`check_zeroinflation()`](https://easystats.github.io/performance/reference/check_zeroinflation.md)
or
[`check_overdispersion()`](https://easystats.github.io/performance/reference/check_overdispersion.md)
are based on simulated residuals. These are usually more accurate for
such tests than the traditionally used Pearson residuals. However, when
simulating from more complex models, such as mixed models or models with
zero-inflation, there are several important considerations.
`simulate_residuals()` relies on
[`DHARMa::simulateResiduals()`](https://rdrr.io/pkg/DHARMa/man/simulateResiduals.html),
and additional arguments specified in `...` are passed further down to
that function. The defaults in DHARMa are set on the most conservative
option that works for all models. However, in many cases, the help
advises to use different settings in particular situations or for
particular models. It is recommended to read the 'Details' in
[`?DHARMa::simulateResiduals`](https://rdrr.io/pkg/DHARMa/man/simulateResiduals.html)
closely to understand the implications of the simulation process and
which arguments should be modified to get the most accurate results.

## References

- Hartig, F., & Lohse, L. (2022). DHARMa: Residual Diagnostics for
  Hierarchical (Multi-Level / Mixed) Regression Models (Version 0.4.5).
  Retrieved from https://CRAN.R-project.org/package=DHARMa

- Dunn, P. K., & Smyth, G. K. (1996). Randomized Quantile Residuals.
  Journal of Computational and Graphical Statistics, 5(3), 236.
  [doi:10.2307/1390802](https://doi.org/10.2307/1390802)

## See also

[`check_residuals()`](https://easystats.github.io/performance/reference/check_residuals.md),
[`check_zeroinflation()`](https://easystats.github.io/performance/reference/check_zeroinflation.md),
[`check_overdispersion()`](https://easystats.github.io/performance/reference/check_overdispersion.md)
and
[`check_predictions()`](https://easystats.github.io/performance/reference/check_predictions.md).
See also
[`see::plot.see_performance_simres()`](https://easystats.github.io/see/reference/plot.see_performance_simres.html)
for options to customize the plot.

## Examples

``` r
m <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
simulate_residuals(m)
#> Simulated residuals from a model of class `lm` based on 250 simulations.
#>   Use `check_residuals()` to check uniformity of residuals or
#>   `residuals()` to extract simulated residuals. It is recommended to refer
#>   to `?DHARMa::simulateResiudals` and `vignette("DHARMa")` for more
#>   information about different settings in particular situations or for
#>   particular models.

# extract residuals
head(residuals(simulate_residuals(m)))
#> [1] 0.356 0.448 0.096 0.568 0.668 0.204
```
