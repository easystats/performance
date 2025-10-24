# Mean Square Error of Linear Models

Compute mean square error of linear models.

## Usage

``` r
performance_mse(model, ...)

mse(model, ...)
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

Numeric, the mean square error of `model`.

## Details

The mean square error is the mean of the sum of squared residuals, i.e.
it measures the average of the squares of the errors. Less technically
speaking, the mean square error can be considered as the variance of the
residuals, i.e. the variation in the outcome the model doesn't explain.
Lower values (closer to zero) indicate better fit.

## Examples

``` r
data(mtcars)
m <- lm(mpg ~ hp + gear, data = mtcars)
performance_mse(m)
#> [1] 8.752858
```
