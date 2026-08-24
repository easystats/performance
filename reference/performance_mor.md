# Median Odds Ratio

A measure of cluster-level variation in multilevel logistic regression,
defined as the median odds ratio between two randomly chosen individuals
from different clusters with identical covariates, comparing the person
at higher risk to the person at lower risk.

## Usage

``` r
performance_mor(x)
```

## Arguments

- x:

  A (logistic) multilevel model.

## Value

A data frame with two columns, one with the group (cluster) names and
one with the median odds ratios.

## Details

The MOR is always greater than or equal to 1 and can be interpreted as
follows:

- *MOR close to 1:* No Cluster Effect. There is (almost) no
  between-cluster heterogeneity, meaning cluster membership plays no
  role in the outcome.

- *MOR \> 1:* Presence of Heterogeneity. Indicates meaningful variation
  across clusters. If two persons are randomly picked from two different
  clusters, the MOR represents the median factor by which the odds of
  the outcome increase for the individual from the higher-risk cluster
  compared to the individual in the lower-risk cluster.

## References

Larsen, K., & Merlo, J. (2005). Appropriate assessment of neighborhood
effects on individual health: integrating random and fixed effects in
multilevel logistic regression. American Journal of Epidemiology,
161(1), 81-88.

## See also

[`performance_ior()`](https://easystats.github.io/performance/reference/performance_ior.md)
and
[`performance_poor()`](https://easystats.github.io/performance/reference/performance_poor.md)
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
performance_mor(m)
#> Median Odds Ratio
#> 
#> Group   |   MOR
#> ---------------
#> Subject | 14.73

m <- suppressWarnings(lme4::glmer(
  high_reaction ~ Days + (1 | mygrp) + (1 | Subject),
  data = sleepstudy,
  family = "binomial"
))
#> boundary (singular) fit: see help('isSingular')
performance_mor(m)
#> boundary (singular) fit: see help('isSingular')
#> Median Odds Ratio
#> 
#> Group   |   MOR
#> ---------------
#> Subject | 14.73
#> mygrp   |  1.00
```
