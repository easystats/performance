# Proper Scoring Rules

Calculates the logarithmic, quadratic/Brier and spherical score from a
model with binary or count outcome.

## Usage

``` r
performance_score(model, verbose = TRUE, ...)
```

## Arguments

- model:

  Model with binary or count outcome.

- verbose:

  Toggle off warnings.

- ...:

  Arguments from other functions, usually only used internally.

## Value

A list with three elements, the logarithmic, quadratic/Brier and
spherical score.

## Details

Proper scoring rules can be used to evaluate the quality of model
predictions and model fit. `performance_score()` calculates the
logarithmic, quadratic/Brier and spherical scoring rules. The spherical
rule takes values in the interval `[0, 1]`, with values closer to 1
indicating a more accurate model, and the logarithmic rule in the
interval `[-Inf, 0]`, with values closer to 0 indicating a more accurate
model.

For
[`stan_lmer()`](https://mc-stan.org/rstanarm/reference/stan_glmer.html)
and
[`stan_glmer()`](https://mc-stan.org/rstanarm/reference/stan_glmer.html)
models, the predicted values are based on
[`posterior_predict()`](https://mc-stan.org/rstantools/reference/posterior_predict.html),
instead of [`predict()`](https://rdrr.io/r/stats/predict.html). Thus,
results may differ more than expected from their non-Bayesian
counterparts in **lme4**.

## Note

Code is partially based on
[GLMMadaptive::scoring_rules()](https://drizopoulos.github.io/GLMMadaptive/reference/scoring_rules.html).

## References

Carvalho, A. (2016). An overview of applications of proper scoring
rules. Decision Analysis 13, 223–242.
[doi:10.1287/deca.2016.0337](https://doi.org/10.1287/deca.2016.0337)

## See also

[`performance_logloss()`](https://easystats.github.io/performance/reference/performance_logloss.md)

## Examples

``` r
## Dobson (1990) Page 93: Randomized Controlled Trial :
counts <- c(18, 17, 15, 20, 10, 20, 25, 13, 12)
outcome <- gl(3, 1, 9)
treatment <- gl(3, 3)
model <- glm(counts ~ outcome + treatment, family = poisson())

performance_score(model)
#> # Proper Scoring Rules
#> 
#> logarithmic: -2.5979
#>   quadratic:  0.2095
#>   spherical:  0.3238
# \donttest{
data(Salamanders, package = "glmmTMB")
model <- glmmTMB::glmmTMB(
  count ~ spp + mined + (1 | site),
  zi = ~ spp + mined,
  family = nbinom2(),
  data = Salamanders
)

performance_score(model)
#> # Proper Scoring Rules
#> 
#> logarithmic:  -1.3275
#>   quadratic: 262.1651
#>   spherical:   0.0316
# }
```
