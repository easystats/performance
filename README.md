
# performance <img src='man/figures/logo.png' align="right" height="139" />

[![Build
Status](https://travis-ci.org/easystats/performance.svg?branch=master)](https://travis-ci.org/easystats/performance)
[![codecov](https://codecov.io/gh/easystats/performance/branch/master/graph/badge.svg)](https://codecov.io/gh/easystats/parameters)
[![HitCount](http://hits.dwyl.io/easystats/performance.svg)](http://hits.dwyl.io/easystats/performance)
[![Documentation](https://img.shields.io/badge/documentation-performance-orange.svg?colorB=E91E63)](https://easystats.github.io/performance/)

***Test if your model is a good model\!***

`performance`’s primary goal is to provide utilities for computing
indices of models fit performance.

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

|    ELPD | ELPD\_SE | LOOIC | LOOIC\_SE | R2\_Median | R2\_MAD | R2\_Mean | R2\_SD | R2\_MAP | R2\_CI\_low | R2\_CI\_high | R2\_LOO\_adjusted |
| ------: | -------: | ----: | --------: | ---------: | ------: | -------: | -----: | ------: | ----------: | -----------: | ----------------: |
| \-78.54 |     4.64 | 157.1 |      9.29 |       0.83 |    0.02 |     0.82 |   0.03 |    0.84 |        0.78 |         0.85 |              0.79 |
