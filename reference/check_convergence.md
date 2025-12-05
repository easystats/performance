# Convergence test for mixed effects models

`check_convergence()` provides an alternative convergence test for
`merMod`-objects.

## Usage

``` r
check_convergence(x, tolerance = 0.001, ...)
```

## Arguments

- x:

  A `merMod` or `glmmTMB`-object.

- tolerance:

  Indicates up to which value the convergence result is accepted. The
  smaller `tolerance` is, the stricter the test will be.

- ...:

  Currently not used.

## Value

`TRUE` if convergence is fine and `FALSE` if convergence is suspicious.
Additionally, the convergence value is returned as attribute.

## Convergence and log-likelihood

Convergence problems typically arise when the model hasn't converged to
a solution where the log-likelihood has a true maximum. This may result
in unreliable and overly complex (or non-estimable) estimates and
standard errors.

## Inspect model convergence

**lme4** performs a convergence-check (see
[`?lme4::convergence`](https://rdrr.io/pkg/lme4/man/convergence.html)),
however, as discussed [here](https://github.com/lme4/lme4/issues/120)
and suggested by one of the lme4-authors in [this
comment](https://github.com/lme4/lme4/issues/120#issuecomment-39920269),
this check can be too strict. `is_converged()` (and its wrapper
function, `performance::check_convergence()`) thus provides an
alternative convergence test for `merMod`-objects.

## Resolving convergence issues

Convergence issues are not easy to diagnose. The help page on
[`?lme4::convergence`](https://rdrr.io/pkg/lme4/man/convergence.html)
provides most of the current advice about how to resolve convergence
issues. In general, convergence issues may be addressed by one or more
of the following strategies: 1. Rescale continuous predictors; 2. try a
different optimizer; 3. increase the number of iterations; or, if
everything else fails, 4. simplify the model. Another clue might be
large parameter values, e.g. estimates (on the scale of the linear
predictor) larger than 10 in (non-identity link) generalized linear
model *might* indicate complete separation, which can be addressed by
regularization, e.g. penalized regression or Bayesian regression with
appropriate priors on the fixed effects.

## Convergence versus Singularity

Note the different meaning between singularity and convergence:
singularity indicates an issue with the "true" best estimate, i.e.
whether the maximum likelihood estimation for the variance-covariance
matrix of the random effects is positive definite or only semi-definite.
Convergence is a question of whether we can assume that the numerical
optimization has worked correctly or not. A convergence failure means
the optimizer (the algorithm) could not find a stable solution (*Bates
et. al 2015*).

For singular models (see
[`?lme4::isSingular`](https://rdrr.io/pkg/lme4/man/isSingular.html)),
convergence is determined based on the optimizer's convergence code. If
the optimizer reports successful convergence (convergence code 0) for a
singular model, `is_converged()` returns `TRUE`. For non-singular
models, in cases where the gradient and Hessian are not available,
`is_converged()` returns `FALSE` and prints a message to indicate that
convergence cannot be assessed through the usual gradient-based checks.
Note that `performance::check_convergence()` is a wrapper around
[`insight::is_converged()`](https://easystats.github.io/insight/reference/is_converged.html).

## References

Bates, D., Mächler, M., Bolker, B., and Walker, S. (2015). Fitting
Linear Mixed-Effects Models Using lme4. Journal of Statistical Software,
67(1), 1-48.
[doi:10.18637/jss.v067.i01](https://doi.org/10.18637/jss.v067.i01)

## See also

Other functions to check model assumptions and and assess model quality:
[`check_autocorrelation()`](https://easystats.github.io/performance/reference/check_autocorrelation.md),
[`check_collinearity()`](https://easystats.github.io/performance/reference/check_collinearity.md),
[`check_heteroscedasticity()`](https://easystats.github.io/performance/reference/check_heteroscedasticity.md),
[`check_homogeneity()`](https://easystats.github.io/performance/reference/check_homogeneity.md),
[`check_model()`](https://easystats.github.io/performance/reference/check_model.md),
[`check_outliers()`](https://easystats.github.io/performance/reference/check_outliers.md),
[`check_overdispersion()`](https://easystats.github.io/performance/reference/check_overdispersion.md),
[`check_predictions()`](https://easystats.github.io/performance/reference/check_predictions.md),
[`check_singularity()`](https://easystats.github.io/performance/reference/check_singularity.md),
[`check_zeroinflation()`](https://easystats.github.io/performance/reference/check_zeroinflation.md)

## Examples

``` r
data(cbpp, package = "lme4")
set.seed(1)
cbpp$x <- rnorm(nrow(cbpp))
cbpp$x2 <- runif(nrow(cbpp))

model <- lme4::glmer(
  cbind(incidence, size - incidence) ~ period + x + x2 + (1 + x | herd),
  data = cbpp,
  family = binomial()
)
#> boundary (singular) fit: see help('isSingular')

check_convergence(model)
#> Could not compute convergence information.
#> [1] NA

# \donttest{
model <- suppressWarnings(glmmTMB::glmmTMB(
  Sepal.Length ~ poly(Petal.Width, 4) * poly(Petal.Length, 4) +
    (1 + poly(Petal.Width, 4) | Species),
  data = iris
))
check_convergence(model)
#> [1] FALSE
# }
```
