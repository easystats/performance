# Ferrari's and Cribari-Neto's R2

Calculates Ferrari's and Cribari-Neto's pseudo R2 (for beta-regression
models).

## Usage

``` r
r2_ferrari(model, ...)

# Default S3 method
r2_ferrari(model, correct_bounds = FALSE, ...)
```

## Arguments

- model:

  Generalized linear, in particular beta-regression model.

- ...:

  Currently not used.

- correct_bounds:

  Logical, whether to correct the bounds of the response variable to
  avoid 0 and 1. If `TRUE`, the response variable is normalized and
  "compressed", i.e. zeros and ones are excluded.

## Value

A list with the pseudo R2 value.

## References

- Ferrari, S., and Cribari-Neto, F. (2004). Beta Regression for
  Modelling Rates and Proportions. Journal of Applied Statistics, 31(7),
  799–815.
  [doi:10.1080/0266476042000214501](https://doi.org/10.1080/0266476042000214501)

## Examples

``` r
data("GasolineYield", package = "betareg")
model <- betareg::betareg(yield ~ batch + temp, data = GasolineYield)
r2_ferrari(model)
#> # R2 for Generalized Linear Regression
#>   Ferrari's R2: 0.962
```
