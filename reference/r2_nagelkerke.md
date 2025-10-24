# Nagelkerke's R2

Calculate Nagelkerke's pseudo-R2.

## Usage

``` r
r2_nagelkerke(model, ...)
```

## Arguments

- model:

  A generalized linear model, including cumulative links resp.
  multinomial models.

- ...:

  Currently not used.

## Value

A named vector with the R2 value.

## References

Nagelkerke, N. J. (1991). A note on a general definition of the
coefficient of determination. Biometrika, 78(3), 691-692.

## Examples

``` r
model <- glm(vs ~ wt + mpg, data = mtcars, family = "binomial")
r2_nagelkerke(model)
#> Nagelkerke's R2 
#>       0.5899593 
```
