# Root Mean Squared Error

Compute root mean squared error for (mixed effects) models, including
Bayesian regression models.

## Usage

``` r
performance_rmse(
  model,
  normalized = FALSE,
  ci = NULL,
  iterations = 100,
  ci_method = NULL,
  verbose = TRUE,
  ...
)

rmse(
  model,
  normalized = FALSE,
  ci = NULL,
  iterations = 100,
  ci_method = NULL,
  verbose = TRUE,
  ...
)
```

## Arguments

- model:

  A model.

- normalized:

  Logical, use `TRUE` if normalized rmse should be returned.

- ci:

  Confidence resp. credible interval level. For
  [`icc()`](https://easystats.github.io/performance/reference/icc.md),
  [`r2()`](https://easystats.github.io/performance/reference/r2.md), and
  `rmse()`, confidence intervals are based on bootstrapped samples from
  the ICC, R2 or RMSE value. See `iterations`.

- iterations:

  Number of bootstrap-replicates when computing confidence intervals for
  the ICC, R2, RMSE etc.

- ci_method:

  Character string, indicating the bootstrap-method. Should be `NULL`
  (default), in which case
  [`lme4::bootMer()`](https://rdrr.io/pkg/lme4/man/bootMer.html) is used
  for bootstrapped confidence intervals. However, if bootstrapped
  intervals cannot be calculated this way, try `ci_method = "boot"`,
  which falls back to
  [`boot::boot()`](https://rdrr.io/pkg/boot/man/boot.html). This may
  successfully return bootstrapped confidence intervals, but
  bootstrapped samples may not be appropriate for the multilevel
  structure of the model. There is also an option
  `ci_method = "analytical"`, which tries to calculate analytical
  confidence assuming a chi-squared distribution. However, these
  intervals are rather inaccurate and often too narrow. It is
  recommended to calculate bootstrapped confidence intervals for mixed
  models.

- verbose:

  Toggle warnings and messages.

- ...:

  Arguments passed down to
  [`lme4::bootMer()`](https://rdrr.io/pkg/lme4/man/bootMer.html) or
  [`boot::boot()`](https://rdrr.io/pkg/boot/man/boot.html) for
  bootstrapped ICC, R2, RMSE etc.; for
  [`variance_decomposition()`](https://easystats.github.io/performance/reference/icc.md),
  arguments are passed down to
  [`brms::posterior_predict()`](https://mc-stan.org/rstantools/reference/posterior_predict.html).

## Value

Numeric, the root mean squared error.

## Details

The RMSE is the square root of the variance of the residuals and
indicates the absolute fit of the model to the data (difference between
observed data to model's predicted values). It can be interpreted as the
standard deviation of the unexplained variance, and is in the same units
as the response variable. Lower values indicate better model fit.

The normalized RMSE is the proportion of the RMSE related to the range
of the response variable. Hence, lower values indicate less residual
variance.

## Examples

``` r
data(Orthodont, package = "nlme")
m <- nlme::lme(distance ~ age, data = Orthodont)

# RMSE
performance_rmse(m, normalized = FALSE)
#> [1] 1.086327

# normalized RMSE
performance_rmse(m, normalized = TRUE)
#> [1] 0.07242178
```
