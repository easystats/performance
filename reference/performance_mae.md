# Mean Absolute Error of Models

Compute mean absolute error of models.

## Usage

``` r
performance_mae(model, ...)

mae(model, ...)
```

## Arguments

- model:

  A model.

- ...:

  Arguments passed down to
  [`lme4::bootMer()`](https://rdrr.io/pkg/lme4/man/bootMer.html) or
  [`boot::boot()`](https://rdrr.io/pkg/boot/man/boot.html) for
  bootstrapped ICC, R2, RMSE etc.; for
  [`variance_decomposition()`](https://easystats.github.io/performance/reference/icc.md),
  arguments are passed down to
  [`brms::posterior_predict()`](https://mc-stan.org/rstantools/reference/posterior_predict.html).

## Value

Numeric, the mean absolute error of `model`.

## Examples

``` r
data(mtcars)
m <- lm(mpg ~ hp + gear, data = mtcars)
performance_mae(m)
#> [1] 2.545822
```
