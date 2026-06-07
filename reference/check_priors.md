# Prior predictive checks

Simulates from the prior marginal distribution of the data to assess the
consistency of the chosen priors with domain knowledge (*Gabry et al.
2019*) and creates a visualization from the prior predictive checks.

## Usage

``` r
check_priors(model = NULL, ...)

# S3 method for class 'stanreg'
check_priors(model = NULL, predictors = NULL, ...)
```

## Arguments

- model:

  A Bayesian model of class `stanreg` or `brmsfit`. Note that this model
  should include draws from the prior predictive distribution. This can
  be achieved, e.g., by using
  [`bayestestR::unupdate()`](https://easystats.github.io/bayestestR/reference/unupdate.html)
  on the fitted model, or setting `prior_PD = TRUE` (when using
  **rstanarm**) or `sample_prior = "only"` (when using **brms**).

- ...:

  Currently not used.

- predictors:

  Character vector with names of one or more model predictors for which
  prior predictive checks should be visualized.

## Value

A data frame of simulated responses and the original response vector.

## Details

Assessing priors based on the prior marginal distribution for the data
provides several methodological advantages:

- It reflects the interplay between the prior distribution on the
  parameters and the likelihood.

- It is a vital component of understanding how prior distributions
  actually work for a given problem.

- It explicitly reflects the idea that we cannot fully understand the
  prior by fixing all except one parameter and assessing the effect of
  the unidimensional marginal prior.

- Instead, we need to assess the effect of the prior as a multivariate
  distribution.

- The prior distribution over the data enables us to extend the concept
  of a weakly informative prior to be more aware of the role of the
  likelihood.

A prior leads to a weakly informative joint prior data-generating
process if draws from the prior data-generating distribution could
represent any data set that could plausibly be observed. Furthermore,
there should be no mass on completely implausible data sets. Generating
simulated data sets can be used to investigate the variability and
multivariate structure of the distribution.

## References

Gabry, J., Simpson, D., Vehtari, A., Betancourt, M., & Gelman, A.
(2019). Visualization in Bayesian Workflow. Journal of the Royal
Statistical Society Series A: Statistics in Society, 182(2), 389–402.
[doi:10.1111/rssa.12378](https://doi.org/10.1111/rssa.12378)

## Examples

``` r
# \dontrun{
model <- insight::download_model("stan_prior_checks_1")
plot(check_priors(model, "mmse"))
#> Error in xy.coords(x, y, xlabel, ylabel, log): 'x' is a list, but does not have components 'x' and 'y'
# }
```
