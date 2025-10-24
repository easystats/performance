# Bayesian R2

Compute R2 for Bayesian models. For mixed models (including a random
part), it additionally computes the R2 related to the fixed effects only
(marginal R2). While `r2_bayes()` returns a single R2 value,
`r2_posterior()` returns a posterior sample of Bayesian R2 values.

## Usage

``` r
r2_bayes(model, robust = TRUE, ci = 0.95, verbose = TRUE, ...)

r2_posterior(model, ...)

# S3 method for class 'brmsfit'
r2_posterior(model, verbose = TRUE, ...)

# S3 method for class 'stanreg'
r2_posterior(model, verbose = TRUE, ...)

# S3 method for class 'BFBayesFactor'
r2_posterior(model, average = FALSE, prior_odds = NULL, verbose = TRUE, ...)
```

## Arguments

- model:

  A Bayesian regression model (from **brms**, **rstanarm**,
  **BayesFactor**, etc).

- robust:

  Logical, if `TRUE`, the median instead of mean is used to calculate
  the central tendency of the variances.

- ci:

  Value or vector of probability of the CI (between 0 and 1) to be
  estimated.

- verbose:

  Toggle off warnings.

- ...:

  Arguments passed to `r2_posterior()`.

- average:

  Compute model-averaged index? See
  [`bayestestR::weighted_posteriors()`](https://easystats.github.io/bayestestR/reference/weighted_posteriors.html).

- prior_odds:

  Optional vector of prior odds for the models compared to the first
  model (or the denominator, for `BFBayesFactor` objects). For
  `data.frame`s, this will be used as the basis of weighting.

## Value

A list with the Bayesian R2 value. For mixed models, a list with the
Bayesian R2 value and the marginal Bayesian R2 value. The standard
errors and credible intervals for the R2 values are saved as attributes.

## Details

`r2_bayes()` returns an "unadjusted" R2 value. See
[`r2_loo()`](https://easystats.github.io/performance/reference/r2_loo.md)
to calculate a LOO-adjusted R2, which comes conceptually closer to an
adjusted R2 measure.

For mixed models, the conditional and marginal R2 are returned. The
marginal R2 considers only the variance of the fixed effects, while the
conditional R2 takes both the fixed and random effects into account.
Technically, since `r2_bayes()` relies on
[`rstantools::bayes_R2()`](https://mc-stan.org/rstantools/reference/bayes_R2.html),
the "marginal" R2 calls `bayes_R2(re.form = NA)`, while the
"conditional" R2 calls `bayes_R2(re.form = NULL)`. The `re.form`
argument is passed to
[`rstantools::posterior_epred()`](https://mc-stan.org/rstantools/reference/posterior_epred.html),
which is internally called in
[`bayes_R2()`](https://mc-stan.org/rstantools/reference/bayes_R2.html).

Note that for "marginal" and "conditional", we refer to the wording
suggested by *Nakagawa et al. 2017*. Thus, we don't use the term
"marginal" in the sense that the random effects are integrated out, but
are "ignored".

`r2_posterior()` is the actual workhorse for `r2_bayes()` and returns a
posterior sample of Bayesian R2 values.

## References

- Gelman, A., Goodrich, B., Gabry, J., and Vehtari, A. (2018). R-squared
  for Bayesian regression models. The American Statistician, 1–6.
  [doi:10.1080/00031305.2018.1549100](https://doi.org/10.1080/00031305.2018.1549100)

- Nakagawa, S., Johnson, P. C. D., and Schielzeth, H. (2017). The
  coefficient of determination R2 and intra-class correlation
  coefficient from generalized linear mixed-effects models revisited and
  expanded. Journal of The Royal Society Interface, 14(134), 20170213.

## Examples

``` r
library(performance)
# \donttest{
model <- suppressWarnings(rstanarm::stan_glm(
  mpg ~ wt + cyl,
  data = mtcars,
  chains = 1,
  iter = 500,
  refresh = 0,
  show_messages = FALSE
))
r2_bayes(model)
#> # Bayesian R2 with Compatibility Interval
#> 
#>   Conditional R2: 0.817 (95% CI [0.724, 0.898])

model <- suppressWarnings(rstanarm::stan_lmer(
  Petal.Length ~ Petal.Width + (1 | Species),
  data = iris,
  chains = 1,
  iter = 500,
  refresh = 0
))
r2_bayes(model)
#> # Bayesian R2 with Compatibility Interval
#> 
#>   Conditional R2: 0.954 (95% CI [0.950, 0.957])
#>      Marginal R2: 0.407 (95% CI [0.203, 0.616])
# }

# \donttest{
model <- suppressWarnings(brms::brm(
  mpg ~ wt + cyl,
  data = mtcars,
  silent = 2,
  refresh = 0
))
#> Error in .fun(model_code = .x1) : 
#>   Boost not found; call install.packages('BH')
#> Error in .fun(model_code = .x1): Boost not found; call install.packages('BH')
r2_bayes(model)
#> # Bayesian R2 with Compatibility Interval
#> 
#>   Conditional R2: 0.954 (95% CI [0.950, 0.957])
#>      Marginal R2: 0.407 (95% CI [0.203, 0.616])

model <- suppressWarnings(brms::brm(
  Petal.Length ~ Petal.Width + (1 | Species),
  data = iris,
  silent = 2,
  refresh = 0
))
#> Error in .fun(model_code = .x1) : 
#>   Boost not found; call install.packages('BH')
#> Error in .fun(model_code = .x1): Boost not found; call install.packages('BH')
r2_bayes(model)
#> # Bayesian R2 with Compatibility Interval
#> 
#>   Conditional R2: 0.954 (95% CI [0.950, 0.957])
#>      Marginal R2: 0.407 (95% CI [0.203, 0.616])
# }
```
