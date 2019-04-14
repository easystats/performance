
# performance <img src='man/figures/logo.png' align="right" height="139" />

[![Build
Status](https://travis-ci.org/easystats/performance.svg?branch=master)](https://travis-ci.org/easystats/performance)
[![codecov](https://codecov.io/gh/easystats/performance/branch/master/graph/badge.svg)](https://codecov.io/gh/easystats/performance)
[![HitCount](http://hits.dwyl.io/easystats/performance.svg)](http://hits.dwyl.io/easystats/performance)
[![Documentation](https://img.shields.io/badge/documentation-performance-orange.svg?colorB=E91E63)](https://easystats.github.io/performance/)

***Test if your model is a good model\!***

The primary goal of the **performance** package is to provide utilities
for computing indices to assess the model quality. This includes
measures like r-squared, root mean squared error or intraclass
correlation coefficient (ICC) , but also functions to check (mixed)
models for overdispersion, zero-inflation, convergence or singularity.

## Installation

Run the following:

``` r
install.packages("devtools")
devtools::install_github("easystats/performance")
```

``` r
library("performance")
```

## Examples

### R-squared and ICC

**performance** has a generic `r2()` function, which computes the
r-squared for many differnt models, including mixed effects and Bayesian
regression models.

``` r
model <- lm(mpg ~ wt + cyl, data = mtcars)
r2(model)
#> $R2
#> [1] 0.8302
#> 
#> $R2_adjusted
#> [1] 0.8185
#> 
#> attr(,"p_value")
#>     value 
#> 6.809e-12 
#> attr(,"F_statistic")
#> value 
#> 70.91 
#> attr(,"DoF")
#> numdf 
#>     2 
#> attr(,"DoF_residual")
#> dendf 
#>    29

model <- glm(am ~ wt + cyl, data = mtcars, family = binomial)
r2(model)
#> $R2_Tjur
#> Tjur's R2 
#>    0.7051

library(MASS)
data(housing)
model <- polr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)
r2(model)
#> $R2_Nagelkerke
#> 'log Lik.' 0.1084 (df=8)
```

### LM

``` r
model <- lm(mpg ~ wt + cyl, data = mtcars)
model_performance(model)
```

| AIC |   BIC |   R2 | R2\_adjusted | X2.44420210815285 |
| --: | ----: | ---: | -----------: | ----------------: |
| 156 | 161.9 | 0.83 |         0.82 |              2.44 |

### GLM

``` r
model <- glm(vs ~ wt + mpg, data = mtcars, family = "binomial")
model_performance(model)
```

|  AIC |  BIC | R2\_Tjur |
| ---: | ---: | -------: |
| 31.3 | 35.7 |     0.48 |

### Bayesian LM (rstanarm)

``` r
library(rstanarm)
model <- rstanarm::stan_glm(mpg ~ wt + cyl, data = mtcars)
model_performance(model)
```

|  AIC |  BIC | R2\_Tjur |
| ---: | ---: | -------: |
| 31.3 | 35.7 |     0.48 |
