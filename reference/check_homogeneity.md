# Check model for homogeneity of variances

Check model for homogeneity of variances between groups described by
independent variables in a model.

## Usage

``` r
check_homogeneity(x, method = "bartlett", ...)

# S3 method for class 'afex_aov'
check_homogeneity(x, method = "levene", ...)
```

## Arguments

- x:

  A linear model or an ANOVA object.

- method:

  Name of the method (underlying test) that should be performed to check
  the homogeneity of variances. May either be `"levene"` for Levene's
  Test for Homogeneity of Variance, `"bartlett"` for the Bartlett test
  (assuming normal distributed samples or groups), `"fligner"` for the
  Fligner-Killeen test (rank-based, non-parametric test), or `"auto"`.
  In the latter case, Bartlett test is used if the model response is
  normal distributed, else Fligner-Killeen test is used.

- ...:

  Arguments passed down to
  [`car::leveneTest()`](https://rdrr.io/pkg/car/man/leveneTest.html).

## Value

Invisibly returns the p-value of the test statistics. A p-value \< 0.05
indicates a significant difference in the variance between the groups.

## Note

There is also a
[[`plot()`](https://rdrr.io/r/graphics/plot.default.html)-method](https://easystats.github.io/see/articles/performance.html)
implemented in the [see-package](https://easystats.github.io/see/).

## See also

Other functions to check model assumptions and and assess model quality:
[`check_autocorrelation()`](https://easystats.github.io/performance/reference/check_autocorrelation.md),
[`check_collinearity()`](https://easystats.github.io/performance/reference/check_collinearity.md),
[`check_convergence()`](https://easystats.github.io/performance/reference/check_convergence.md),
[`check_heteroscedasticity()`](https://easystats.github.io/performance/reference/check_heteroscedasticity.md),
[`check_model()`](https://easystats.github.io/performance/reference/check_model.md),
[`check_outliers()`](https://easystats.github.io/performance/reference/check_outliers.md),
[`check_overdispersion()`](https://easystats.github.io/performance/reference/check_overdispersion.md),
[`check_predictions()`](https://easystats.github.io/performance/reference/check_predictions.md),
[`check_singularity()`](https://easystats.github.io/performance/reference/check_singularity.md),
[`check_zeroinflation()`](https://easystats.github.io/performance/reference/check_zeroinflation.md)

## Examples

``` r
model <<- lm(len ~ supp + dose, data = ToothGrowth)
check_homogeneity(model)
#> OK: There is not clear evidence for different variances across groups (Bartlett Test, p = 0.226).
#> 

# plot results
result <- check_homogeneity(model)
plot(result)
```
