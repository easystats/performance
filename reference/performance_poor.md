# Proportion of Opposed Odds Ratios

This function quantifies the extent to which the residual (unexplained)
stratum heterogeneity dominates the estimated fixed effect of a group-
or stratum-level characteristic. This measure represents the percentage
of all hypothetical, pairwise comparisons in which the individual-level
effect is in the opposite direction of the estimated average odds ratio
(OR).

## Usage

``` r
performance_poor(x)
```

## Arguments

- x:

  A (logistic) multilevel model.

## Value

A data frame with the parameter names and their POOR estimates.

## Details

The POOR value always ranges between 0% and 50%.

- 0%: Perfect homogeneity. Every single randomly selected pair of
  observations shows an effect in the same direction as the overall OR.

- 50%: Total heterogeneity (pure chance). In exactly half of the cases,
  the effect is reversed; the stratum characteristic possesses no
  orderly predictive power whatsoever, as it is completely dominated by
  the unexplained stratum variance.

Practical Significance: A high POOR value (e.g., \> 30%) serves as a
strong warning to researchers against making overgeneralized statements
about the utility of a stratum characteristic, as the average OR masks
massive internal heterogeneity.

## See also

[`performance_ior()`](https://easystats.github.io/performance/reference/performance_ior.md)
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
performance_poor(m)
#> Proportion of Opposed Odds Ratios
#> 
#> Parameter   |   Group | POOR
#> ----------------------------
#> (Intercept) | Subject | 0.19
#> Days        | Subject | 0.43

m <- suppressWarnings(lme4::glmer(
  high_reaction ~ Days + (1 | mygrp) + (1 | Subject),
  data = sleepstudy,
  family = "binomial"
))
performance_poor(m)
#> Proportion of Opposed Odds Ratios
#> 
#> Parameter   |   Group |     POOR
#> --------------------------------
#> (Intercept) | Subject |     0.20
#> Days        | Subject |     0.43
#> (Intercept) |   mygrp | 7.40e-12
#> Days        |   mygrp |     0.07
```
