
# performance <img src='man/figures/logo.png' align="right" height="139" />

[![CRAN](http://www.r-pkg.org/badges/version/performance)](https://cran.r-project.org/package=performance)
[![downloads](http://cranlogs.r-pkg.org/badges/performance)](https://cran.r-project.org/package=performance)
[![Build
Status](https://travis-ci.org/easystats/performance.svg?branch=master)](https://travis-ci.org/easystats/performance)
[![codecov](https://codecov.io/gh/easystats/performance/branch/master/graph/badge.svg)](https://codecov.io/gh/easystats/performance)

***Test if your model is a good model\!***

The primary goal of the **performance** package is to provide utilities
for computing **indices of model quality** and **goodness of fit**. This
includes measures like r-squared, root mean squared error or intraclass
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

# Examples

[![Documentation](https://img.shields.io/badge/documentation-performance-orange.svg?colorB=E91E63)](https://easystats.github.io/performance/)
[![Blog](https://img.shields.io/badge/blog-easystats-orange.svg?colorB=FF9800)](https://easystats.github.io/blog/posts/)
[![Features](https://img.shields.io/badge/features-performance-orange.svg?colorB=2196F3)](https://easystats.github.io/performance/reference/index.html)

## Assessing model quality

### R-squared

**performance** has a generic `r2()` function, which computes the
r-squared for many different models, including mixed effects and
Bayesian regression models.

`r2()` returns a list containing values related to the “most
appropriate” r-squared for the given model.

``` r
model <- lm(mpg ~ wt + cyl, data = mtcars)
r2(model)
#> # R2 for Linear Regression
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

For frequentist mixed models, `r2()` (resp. `r2_nakagawa()`) computes
the *mean* random effect variances, thus `r2()` is also appropriate for
mixed models with more complex random effects structures, like random
slopes or nested random effects (see *Johnson 2014* and *Nakagawa et
al. 2017*).

``` r
library(rstanarm)
model <- stan_glmer(Petal.Length ~ Petal.Width + (1 | Species), 
    data = iris, cores = 4)
r2(model)
#> # Bayesian R2 with Standard Error
#> 
#>   Conditional R2: 0.954 [0.002]
#>      Marginal R2: 0.410 [0.123]

library(lme4)
model <- lmer(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)
r2(model)
#> # R2 for mixed models
#> 
#>   Conditional R2: 0.799
#>      Marginal R2: 0.279
```

### Intraclass Correlation Coefficient (ICC)

Similar to r-squared, the ICC provides information on the explained
variance and can be interpreted as “the proportion of the variance
explained by the grouping structure in the population” (*Hox 2010: 15*).

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
#> # Random Effect Variances and ICC
#> 
#> Conditioned on: all random effects
#> 
#> ## Variance Ratio (comparable to ICC)
#> Ratio: 0.39  CI 95%: [-0.55 0.78]
#> 
#> ## Variances of Posterior Predicted Distribution
#> Conditioned on fixed effects: 22.96  CI 95%: [ 8.52 58.02]
#> Conditioned on rand. effects: 37.84  CI 95%: [24.84 56.64]
#> 
#> ## Difference in Variances
#> Difference: 14.21  CI 95%: [-19.82 35.66]
```

## Model diagnostics

### Check for overdispersion

Overdispersion occurs when the observed variance in the data is higher
than the expected variance from the model assumption (for Poisson,
variance roughly equals the mean of an outcome).
`check_overdispersion()` checks if a count model (including mixed
models) is overdispersed or not.

``` r
library(glmmTMB)
data(Salamanders)
model <- glm(count ~ spp + mined, family = poisson, data = Salamanders)
check_overdispersion(model)
#> # Overdispersion test
#> 
#>        dispersion ratio =    2.946
#>   Pearson's Chi-Squared = 1873.710
#>                 p-value =  < 0.001
#> Overdispersion detected.
```

Overdispersion can be fixed by either modelling the dispersion parameter
(not possible with all packages), or by choosing a different
distributional family (like Quasi-Poisson, or negative binomial, see
*Gelman and Hill 2007*).

### Check for zero-inflation

Zero-inflation (in (Quasi-)Poisson models) is indicated when the amount
of observed zeros is larger than the amount of predicted zeros, so the
model is *underfitting* zeros. In such cases, it is recommended to use
negative binomial or zero-inflated models.

Use `check_zeroinflation()` to check if zero-inflation is present in the
fitted model.

``` r
model <- glm(count ~ spp + mined, family = poisson, data = Salamanders)
check_zeroinflation(model)
#> # Check for zero-inflation
#> 
#>    Observed zeros: 387
#>   Predicted zeros: 298
#>             Ratio: 0.77
#> Model is underfitting zeros (probable zero-inflation).
```

### Check for singular model fits

A “singular” model fit means that some dimensions of the
variance-covariance matrix have been estimated as exactly zero. This
often occurs for mixed models with overly complex random effects
structures.

`check_singularity()` checks mixed models (of class `lme`, `merMod`,
`glmmTMB` or `MixMod`) for singularity, and returns `TRUE` if the model
fit is singular.

``` r
library(lme4)
data(sleepstudy)

# prepare data
set.seed(123)
sleepstudy$mygrp <- sample(1:5, size = 180, replace = TRUE)
sleepstudy$mysubgrp <- NA
for (i in 1:5) {
    filter_group <- sleepstudy$mygrp == i
    sleepstudy$mysubgrp[filter_group] <- sample(1:30, size = sum(filter_group), 
        replace = TRUE)
}

# fit strange model
model <- lmer(Reaction ~ Days + (1 | mygrp/mysubgrp) + (1 | Subject), 
    data = sleepstudy)

check_singularity(model)
#> [1] TRUE
```

Remedies to cure issues with singular fits can be found
[here](https://easystats.github.io/performance/reference/check_singularity.html).

## Model performance summaries

`model_performance()` computes indices of model performance for
regression models. Depending on the model object, typical indices might
be r-squared, AIC, BIC, RMSE, ICC or LOOIC.

### Linear model

``` r
m1 <- lm(mpg ~ wt + cyl, data = mtcars)
model_performance(m1)
```

| AIC |   BIC |   R2 | R2\_adjusted | RMSE |
| --: | ----: | ---: | -----------: | ---: |
| 156 | 161.9 | 0.83 |         0.82 | 2.44 |

### Logistic regression

``` r
m2 <- glm(vs ~ wt + mpg, data = mtcars, family = "binomial")
model_performance(m2)
```

|  AIC |  BIC | R2\_Tjur | RMSE | LOGLOSS | SCORE\_LOG | SCORE\_SPHERICAL |  PCP |
| ---: | ---: | -------: | ---: | ------: | ---------: | ---------------: | ---: |
| 31.3 | 35.7 |     0.48 | 0.89 |     0.4 |     \-14.9 |             0.09 | 0.74 |

### Linear mixed model

``` r
library(lme4)
m3 <- lmer(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)
model_performance(m3)
```

|  AIC |  BIC | R2\_conditional | R2\_marginal |  ICC |  RMSE |
| ---: | ---: | --------------: | -----------: | ---: | ----: |
| 1756 | 1775 |             0.8 |         0.28 | 0.72 | 23.44 |

### Comparing different models

``` r
counts <- c(18, 17, 15, 20, 10, 20, 25, 13, 12)
outcome <- gl(3, 1, 9)
treatment <- gl(3, 3)
m4 <- glm(counts ~ outcome + treatment, family = poisson())

compare_performance(m1, m2, m3, m4)
```

| name | class   |     AIC |     BIC |  RMSE | SCORE\_LOG | SCORE\_SPHERICAL |   R2 | R2\_adjusted | R2\_Tjur | LOGLOSS |  PCP | R2\_conditional | R2\_marginal |  ICC | R2\_Nagelkerke |
| :--- | :------ | ------: | ------: | ----: | ---------: | ---------------: | ---: | -----------: | -------: | ------: | ---: | --------------: | -----------: | ---: | -------------: |
| m1   | lm      |  156.01 |  161.87 |  2.44 |            |                  | 0.83 |         0.82 |          |         |      |                 |              |      |                |
| m2   | glm     |   31.30 |   35.70 |  0.89 |     \-14.9 |             0.09 |      |              |     0.48 |     0.4 | 0.74 |                 |              |      |                |
| m3   | lmerMod | 1755.63 | 1774.79 | 23.44 |            |                  |      |              |          |         |      |             0.8 |         0.28 | 0.72 |                |
| m4   | glm     |   56.76 |   57.75 |  0.75 |      \-2.6 |             0.32 |      |              |          |         |      |                 |              |      |           0.66 |

# References

Gelman, A., & Hill, J. (2007). Data analysis using regression and
multilevel/hierarchical models. Cambridge; New York: Cambridge
University Press.

Hox, J. J. (2010). Multilevel analysis: techniques and applications (2nd
ed). New York: Routledge.

Johnson, P. C. D. (2014). Extension of Nakagawa & Schielzeth’s R2 GLMM
to random slopes models. Methods in Ecology and Evolution, 5(9),
944–946. <https://doi.org/10.1111/2041-210X.12225>

Nakagawa, S., Johnson, P. C. D., & Schielzeth, H. (2017). The
coefficient of determination R2 and intra-class correlation coefficient
from generalized linear mixed-effects models revisited and expanded.
Journal of The Royal Society Interface, 14(134), 20170213.
<https://doi.org/10.1098/rsif.2017.0213>
