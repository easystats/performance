# McFadden's R2

Calculates McFadden's pseudo R2.

## Usage

``` r
r2_mcfadden(model, ...)
```

## Arguments

- model:

  Generalized linear or multinomial logit (`mlogit`) model.

- ...:

  Currently not used.

## Value

For most models, a list with McFadden's R2 and adjusted McFadden's R2
value. For some models, only McFadden's R2 is available.

## References

- McFadden, D. (1987). Regression-based specification tests for the
  multinomial logit model. Journal of econometrics, 34(1-2), 63-82.

- McFadden, D. (1973). Conditional logit analysis of qualitative choice
  behavior.

## Examples

``` r
if (require("mlogit")) {
  data("Fishing", package = "mlogit")
  Fish <- mlogit.data(Fishing, varying = c(2:9), shape = "wide", choice = "mode")

  model <- mlogit(mode ~ price + catch, data = Fish)
  r2_mcfadden(model)
}
#> Loading required package: mlogit
#> Loading required package: dfidx
#> McFadden's R2 
#>       0.17823 
```
