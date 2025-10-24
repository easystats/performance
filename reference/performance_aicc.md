# Compute the AIC or second-order AIC

Compute the AIC or the second-order Akaike's information criterion
(AICc). `performance_aic()` is a small wrapper that returns the AIC,
however, for models with a transformed response variable,
`performance_aic()` returns the corrected AIC value (see 'Examples'). It
is a generic function that also works for some models that don't have a
AIC method (like Tweedie models). `performance_aicc()` returns the
second-order (or "small sample") AIC that incorporates a correction for
small sample sizes.

## Usage

``` r
performance_aicc(x, ...)

performance_aic(x, ...)

# Default S3 method
performance_aic(x, estimator = "ML", verbose = TRUE, ...)

# S3 method for class 'lmerMod'
performance_aic(x, estimator = "REML", verbose = TRUE, ...)
```

## Arguments

- x:

  A model object.

- ...:

  Currently not used.

- estimator:

  Only for linear models. Corresponds to the different estimators for
  the standard deviation of the errors. If `estimator = "ML"` (default,
  except for `performance_aic()` when the model object is of class
  `lmerMod`), the scaling is done by `n` (the biased ML estimator),
  which is then equivalent to using `AIC(logLik())`. Setting it to
  `"REML"` will give the same results as
  `AIC(logLik(..., REML = TRUE))`.

- verbose:

  Toggle warnings.

## Value

Numeric, the AIC or AICc value.

## Details

`performance_aic()` correctly detects transformed response and, unlike
[`stats::AIC()`](https://rdrr.io/r/stats/AIC.html), returns the
"corrected" AIC value on the original scale. To get back to the original
scale, the likelihood of the model is multiplied by the
Jacobian/derivative of the transformation.

In case it is not possible to return the corrected AIC value, a warning
is given that the corrected log-likelihood value could not be computed.

## References

- Akaike, H. (1973) Information theory as an extension of the maximum
  likelihood principle. In: Second International Symposium on
  Information Theory, pp. 267-281. Petrov, B.N., Csaki, F., Eds,
  Akademiai Kiado, Budapest.

- Hurvich, C. M., Tsai, C.-L. (1991) Bias of the corrected AIC criterion
  for underfitted regression and time series models. Biometrika 78,
  499–509.

## Examples

``` r
m <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
AIC(m)
#> [1] 159.1051
performance_aicc(m)
#> [1] 162.4651

# correct AIC for models with transformed response variable
data("mtcars")
mtcars$mpg <- floor(mtcars$mpg)
model <- lm(log(mpg) ~ factor(cyl), mtcars)

# wrong AIC, not corrected for log-transformation
AIC(model)
#> [1] -19.67061

# performance_aic() correctly detects transformed response and
# returns corrected AIC
performance_aic(model)
#> [1] 168.2152

# \dontrun{
# there are a few exceptions where the corrected log-likelihood values
# cannot be returned. The following exampe gives a warning.
model <- lm(1 / mpg ~ factor(cyl), mtcars)
performance_aic(model)
#> Warning: Could not compute corrected log-likelihood for models with transformed
#>   response. Log-likelihood value is probably inaccurate.
#> [1] -196.3387
# }
```
