# Interval Odds Ratio

The Interval Odds Ratio (IOR) evaluates the fixed effect of a
cluster-level (level 2) covariate by explicitly incorporating the
residual between-cluster heterogeneity. `interval_odds_ratio()` is an
alias for `performance_ior()`.

## Usage

``` r
performance_ior(x)

interval_odds_ratio(x)
```

## Arguments

- x:

  A (logistic) multilevel model.

## Value

A data frame with the parameter names and their interval odds ratios.

## Details

Unlike a standard confidence interval (which reflects sample estimation
uncertainty around the coefficients), the IOR reflects the variation in
odds ratios across clusters due to residual cluster heterogeneity.

- *IOR does not contain 1:* If the entire interval is above 1 (or below
  1), the cluster-level covariate has a strong effect. Even when moving
  from a "good" unexposed cluster to a "bad" exposed cluster (or vice
  versa), the directional effect of the covariate remains dominant.

- *IOR contains 1:* When the interval contains 1, the between-cluster
  heterogeneity is larger than the effect of the covariate itself. This
  means an individual moving from an unexposed cluster to an exposed
  cluster could actually experience lower odds of the outcome if the new
  cluster happens to have a very low unobserved random effect.

## References

Larsen K, Merlo J. Appropriate Assessment of Neighborhood Effects on
Individual Health: Integrating Random and Fixed Effects in Multilevel
Logistic Regression. American Journal of Epidemiology (2005) 161:81–88.
[doi:10.1093/aje/kwi017](https://doi.org/10.1093/aje/kwi017)

Merlo J, Wagner P, Ghith N, Leckie G. An Original Stepwise Multilevel
Logistic Regression Analysis of Discriminatory Accuracy: The Case of
Neighbourhoods and Health. PLoS ONE (2016) 11:e0153778.
[doi:10.1371/journal.pone.0153778](https://doi.org/10.1371/journal.pone.0153778)

## See also

[`performance_poor()`](https://easystats.github.io/performance/reference/performance_poor.md)
and
[`performance_mor()`](https://easystats.github.io/performance/reference/performance_mor.md)
as additional metrics specifically for logistic multilevel regression
models, and
[`icc()`](https://easystats.github.io/performance/reference/icc.md) for
multilevel models in general.

## Examples

``` r
data(sleepstudy, package = "lme4")
sleepstudy$mygrp <- sample(1:5, size = 180, replace = TRUE)
sleepstudy$high_reaction <- as.factor(datawizard::categorize(sleepstudy$Reaction))

m <- lme4::glmer(
  high_reaction ~ Days + (1 | Subject),
  data = sleepstudy,
  family = "binomial"
)
performance_ior(m)
#> Interval Odds Ratio
#> 
#> Parameter   |   Group |         80% CI
#> --------------------------------------
#> (Intercept) | Subject | [0.00,   5.21]
#> Days        | Subject | [0.01, 348.24]

m <- suppressWarnings(lme4::glmer(
  high_reaction ~ Days + (1 | mygrp) + (1 | Subject),
  data = sleepstudy,
  family = "binomial"
))
#> boundary (singular) fit: see help('isSingular')
performance_ior(m)
#> boundary (singular) fit: see help('isSingular')
#> Interval Odds Ratio
#> 
#> Parameter   |   Group |         80% CI
#> --------------------------------------
#> (Intercept) | Subject | [0.00,   5.21]
#> Days        | Subject | [0.01, 348.24]
#> (Intercept) |   mygrp | [0.03,   0.03]
#> Days        |   mygrp | [2.10,   2.10]
```
