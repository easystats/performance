# Check model for violation of sphericity

Check model for violation of sphericity. For [Bartlett's Test of
Sphericity](https://easystats.github.io/performance/reference/check_factorstructure.md)
(used for correlation matrices and factor analyses), see
[check_sphericity_bartlett](https://easystats.github.io/performance/reference/check_factorstructure.md).

## Usage

``` r
check_sphericity(x, ...)
```

## Arguments

- x:

  A model object.

- ...:

  Arguments passed to
  [`car::Anova`](https://rdrr.io/pkg/car/man/Anova.html).

## Value

Invisibly returns the p-values of the test statistics. A p-value \< 0.05
indicates a violation of sphericity.

## Examples

``` r
data(Soils, package = "carData")
soils.mod <- lm(
  cbind(pH, N, Dens, P, Ca, Mg, K, Na, Conduc) ~ Block + Contour * Depth,
  data = Soils
)

check_sphericity(Manova(soils.mod))
#> OK: Data seems to be spherical (p > .999).
#> 
```
