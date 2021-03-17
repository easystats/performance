---
title: "Extracting, Computing and Exploring the Parameters of Statistical Models using R"
authors:
- affiliation: 1
  name: Daniel Lüdecke
  orcid: 0000-0002-8895-3206
- affiliation: 2
  name: Mattan S. Ben-Shachar
  orcid: 0000-0002-4287-4801
- affiliation: 3
  name: Indrajeet Patil
  orcid: 0000-0003-1995-6531
- affiliation: 4
  name: Philip Waggoner
  orcid: 0000-0002-7825-7573
- affiliation: 5
  name: Dominique Makowski
  orcid: 0000-0001-5375-9967
date: "21 March 2021"
output: pdf_document
bibliography: paper.bib
csl: apa.csl
tags:
- R
- easystats
- parameters
- regression
- linear models
- coefficients
affiliations:
- index: 1
  name:  University Medical Center Hamburg-Eppendorf, Germany
- index: 2
  name: Ben-Gurion University of the Negev, Israel
- index: 3
  name: Center for Humans and Machines, Max Planck Institute for Human Development, Berlin, Germany
- index: 4
  name: University of Chicago, USA  
- index: 5
  name: Nanyang Technological University, Singapore
---

# Summary

The `performance` package provides utilities for computing measures to assess
model quality, which are not directly provided by R's `base` or `stats`
packages. These include e.g. measures like $R^2$, intraclass correlation
coefficient, root mean squared error, etc., or functions to check models for
overdispersion, singularity or zero-inflation and more. Functions apply to a
large variety of regression models, including generalized linear models, mixed
effects models, and Bayesian models.

# Aims of the Package

**performance** is part of the
[*easystats*](https://github.com/easystats/performance) ecosystem, a
collaborative project created to facilitate the usage of R for statistical
analyses.

# Comparison to other Packages

# Examples of Features

## Assessing Model Quality

The `model_performance()` function is the workhorse of this package and allows
you to extract a comprehensive set of model fit indices from various models in a
consistent way. Depending on the regression model object, the list of computed
indices might include $R^2$, AIC, BIC, RMSE, ICC, LOOIC, etc.

Example with linear model

``` r
m1 <- lm(mpg ~ wt + cyl, data = mtcars)
model_performance(m1)
#> # Indices of model performance
#> 
#> AIC     |     BIC |    R2 | R2 (adj.) |  RMSE | Sigma
#> -----------------------------------------------------
#> 156.010 | 161.873 | 0.830 |     0.819 | 2.444 | 2.568
```

Example with logistic regression

``` r
m2 <- glm(vs ~ wt + mpg, data = mtcars, family = "binomial")
model_performance(m2)
#> # Indices of model performance
#> 
#> AIC    |    BIC | Tjur's R2 |  RMSE | Sigma | Log_loss | Score_log | Score_spherical |   PCP
#> --------------------------------------------------------------------------------------------
#> 31.298 | 35.695 |     0.478 | 0.359 | 0.934 |    0.395 |   -14.903 |           0.095 | 0.743
```

Example with linear mixed model:

``` r
library(lme4)
m3 <- lmer(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)
model_performance(m3)
#> # Indices of model performance
#> 
#> AIC      |      BIC | R2 (cond.) | R2 (marg.) |   ICC |   RMSE |  Sigma
#> -----------------------------------------------------------------------
#> 1755.628 | 1774.786 |      0.799 |      0.279 | 0.722 | 23.438 | 25.592
```

## Visualisation

**performance** functions also include plotting capabilities via the [**see** package](https://easystats.github.io/see/) [@ludecke2020see]. A complete
overview of plotting functions is available at the *see* website
(https://easystats.github.io/see/articles/performance.html).

## Visual Check of Model Assumptions

```r
library(see)
library(lme4)
data(sleepstudy)

model <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
check_model(model)
```

## Visual Comparison of Model Fits

```r
library(see)
data(iris)

lm1 <- lm(Sepal.Length ~ Species, data = iris)
lm2 <- lm(Sepal.Length ~ Species + Petal.Length, data = iris)
lm3 <- lm(Sepal.Length ~ Species * Sepal.Width, data = iris)
lm4 <- lm(Sepal.Length ~ Species * Sepal.Width + Petal.Length + Petal.Width, data = iris)

plot(compare_performance(lm1, lm2, lm3, lm4))
```

![](figure2.png)

# Licensing and Availability

**performance** is licensed under the GNU General Public License (v3.0), with
all source code stored at GitHub (https://github.com/easystats/performance), and
with a corresponding issue tracker for bug reporting and feature enhancements.
In the spirit of honest and open science, we encourage requests/tips for fixes,
feature updates, as well as general questions and concerns via direct
interaction with contributors and developers.

# Acknowledgments

**performance** is part of the collaborative
[*easystats*](https://github.com/easystats/easystats) ecosystem. Thus, we would
like to thank the [members of easystats](https://github.com/orgs/easystats/people) 
as well as the users.

# References
