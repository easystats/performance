
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

### R-squared

**performance** has a generic `r2()` function, which computes the
r-squared for many differnt models, including mixed effects and Bayesian
regression models.

`r2()` returns a list containing values related to the “most
appropriate” r-squared for the given model.

``` r
model <- lm(mpg ~ wt + cyl, data = mtcars)
r2(model)
#> # R2 for linear models
#> 
#>        R2: 0.830
#>   adj. R2: 0.819

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
#> Nagelkerke's R2 
#>          0.1084
```

The different r-squared measures can also be accessed directly via
functions like `r2_bayes()`, `r2_coxsnell()` or `r2_nagelkerke()` (see a
full list of functions
[here](https://easystats.github.io/performance/reference/index.html)).

For mixed models, the *conditional* and *marginal* r-squared are
returned. The *marginal r-squared* considers only the variance of the
fixed effects and indicates how much of the model’s variance is
explained by the fixed effects part only. The *conditional r-squared*
takes both the fixed and random effects into account and indicates how
much of the model’s variance is explained by the “complete” model.

For frequentist mixed models, the random effect variances are the mean
random effect variances, thus the r-squared value is also appropriate
for mixed models with random slopes or nested random effects (see
*Johnson 2014* and *Nakagawa et al. 2017*).

``` r
library(rstanarm)
model <- stan_glmer(Petal.Length ~ Petal.Width + (1 | Species), 
    data = iris, cores = 4)
r2(model)
#> # Bayesian R2 with Standard Error
#> 
#>   Conditional R2: 0.954 [0.002]
#>      Marginal R2: 0.408 [0.120]

library(lme4)
model <- lmer(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)
r2(model)
#> # R2 for mixed models
#> 
#>   Conditional R2: 0.799
#>      Marginal R2: 0.279
```

### Intraclass Correlation Coefficient (ICC)

Similar to r-squared, the ICC also provides information on the explained
variance and can be interpreted as “the proportion of the variance
explained by the grouping structure in the population” (*Hox 2002: 15*).

`icc()` calculates the ICC for various mixed model objects, including
`stanreg` models.

``` r
library(lme4)
model <- lmer(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)
icc(model)
#> # Intraclass Correlation Coefficient
#> 
#>      Adjusted ICC: 0.722
#>   Conditional ICC: 0.521
```

For models of class `brmsfit`, an ICC based on variance decomposition is
returned (for details, see the
[documentation](https://easystats.github.io/performance/reference/icc.html)).

``` r
library(brms)
set.seed(123)
model <- brm(mpg ~ wt + (1 | cyl) + (1 + wt | gear), data = mtcars)
```

``` r
icc(model)
#> 
#> # Random Effect Variances and ICC
#> 
#> Conditioned on: all random effects
#> 
#> ## Variance Ratio (comparable to ICC)
#> Ratio: 0.39  CI 95%: [-0.58 0.78]
#> 
#> ## Variances of Posterior Predicted Distribution
#> Conditioned on fixed effects: 22.82  CI 95%: [ 8.29 58.51]
#> Conditioned on rand. effects: 37.72  CI 95%: [25.39 56.79]
#> 
#> ## Difference in Variances
#> Difference: 14.39  CI 95%: [-20.39 35.76]
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

# References

Gelman, A., Goodrich, B., Gabry, J., & Vehtari, A. (2018). R-squared for
Bayesian regression models. The American Statistician, 1–6. doi:
10.1080/00031305.2018.1549100

Hox J. 2002. Multilevel analysis: techniques and applications. Mahwah,
NJ: Erlbaum

Johnson PC, O’Hara RB. 2014. Extension of Nakagawa & Schielzeth’s R2GLMM
to random slopes models. Methods Ecol Evol, 5: 944-946. (doi:
10.1111/2041-210X.12225)

Nakagawa S, Johnson P, Schielzeth H (2017) The coefficient of
determination R2 and intra-class correlation coefficient from
generalized linear mixed-effects models revisted and expanded. J. R.
Soc. Interface 14. doi: 10.1098/rsif.2017.0213
