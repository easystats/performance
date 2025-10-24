# Kullback-Leibler R2

Calculates the Kullback-Leibler-divergence-based R2 for generalized
linear models.

## Usage

``` r
r2_kullback(model, ...)

# S3 method for class 'glm'
r2_kullback(model, adjust = TRUE, ...)
```

## Arguments

- model:

  A generalized linear model.

- ...:

  Additional arguments. Currently not used.

- adjust:

  Logical, if `TRUE` (the default), the adjusted R2 value is returned.

## Value

A named vector with the R2 value.

## References

Cameron, A. C. and Windmeijer, A. G. (1997) An R-squared measure of
goodness of fit for some common nonlinear regression models. Journal of
Econometrics, 77: 329-342.

## Examples

``` r
model <- glm(vs ~ wt + mpg, data = mtcars, family = "binomial")
r2_kullback(model)
#> Kullback-Leibler R2 
#>           0.3834362 
```
