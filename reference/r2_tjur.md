# Tjur's R2 - coefficient of determination (D)

This method calculates the Coefficient of Discrimination `D` (also known
as Tjur's R2; Tjur, 2009) for generalized linear (mixed) models for
binary outcomes. It is an alternative to other pseudo-R2 values like
Nagelkerke's R2 or Cox-Snell R2. The Coefficient of Discrimination `D`
can be read like any other (pseudo-)R2 value.

## Usage

``` r
r2_tjur(model, ...)
```

## Arguments

- model:

  Binomial Model.

- ...:

  Arguments from other functions, usually only used internally.

## Value

A named vector with the R2 value.

## References

Tjur, T. (2009). Coefficients of determination in logistic regression
models - A new proposal: The coefficient of discrimination. The American
Statistician, 63(4), 366-372.

## Examples

``` r
model <- glm(vs ~ wt + mpg, data = mtcars, family = "binomial")
r2_tjur(model)
#> Tjur's R2 
#> 0.4776926 
```
