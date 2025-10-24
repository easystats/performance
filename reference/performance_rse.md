# Residual Standard Error for Linear Models

Compute residual standard error of linear models.

## Usage

``` r
performance_rse(model)
```

## Arguments

- model:

  A model.

## Value

Numeric, the residual standard error of `model`.

## Details

The residual standard error is the square root of the residual sum of
squares divided by the residual degrees of freedom.

## Examples

``` r
data(mtcars)
m <- lm(mpg ~ hp + gear, data = mtcars)
performance_rse(m)
#> [1] 3.107785
```
