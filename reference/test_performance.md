# Test if models are different

Testing whether models are "different" in terms of accuracy or
explanatory power is a delicate and often complex procedure, with many
limitations and prerequisites. Moreover, many tests exist, each coming
with its own interpretation, and set of strengths and weaknesses.

The `test_performance()` function runs the most relevant and appropriate
tests based on the type of input (for instance, whether the models are
*nested* or not). However, it still requires the user to understand what
the tests are and what they do in order to prevent their
misinterpretation. See the *Details* section for more information
regarding the different tests and their interpretation.

## Usage

``` r
test_bf(...)

# Default S3 method
test_bf(..., reference = 1, text_length = NULL)

test_likelihoodratio(..., estimator = "ML", verbose = TRUE)

test_lrt(..., estimator = "ML", verbose = TRUE)

test_performance(..., reference = 1, verbose = TRUE)

test_vuong(..., verbose = TRUE)

test_wald(..., verbose = TRUE)
```

## Arguments

- ...:

  Multiple model objects.

- reference:

  This only applies when models are non-nested, and determines which
  model should be taken as a reference, against which all the other
  models are tested.

- text_length:

  Numeric, length (number of chars) of output lines. `test_bf()`
  describes models by their formulas, which can lead to overly long
  lines in the output. `text_length` fixes the length of lines to a
  specified limit.

- estimator:

  Applied when comparing regression models using
  `test_likelihoodratio()`. Corresponds to the different estimators for
  the standard deviation of the errors. Defaults to `"OLS"` for linear
  models, `"ML"` for all other models (including mixed models), or
  `"REML"` for linear mixed models when these have the same fixed
  effects. See 'Details'.

- verbose:

  Toggle warning and messages.

## Value

A data frame containing the relevant indices.

## Details

### Nested vs. Non-nested Models

Model's "nesting" is an important concept of models comparison. Indeed,
many tests only make sense when the models are *"nested",* i.e., when
their predictors are nested. This means that all the *fixed effects*
predictors of a model are contained within the *fixed effects*
predictors of a larger model (sometimes referred to as the encompassing
model). For instance, `model1 (y ~ x1 + x2)` is "nested" within
`model2 (y ~ x1 + x2 + x3)`. Usually, people have a list of nested
models, for instance `m1 (y ~ 1)`, `m2 (y ~ x1)`, `m3 (y ~ x1 + x2)`,
`m4 (y ~ x1 + x2 + x3)`, and it is conventional that they are "ordered"
from the smallest to largest, but it is up to the user to reverse the
order from largest to smallest. The test then shows whether a more
parsimonious model, or whether adding a predictor, results in a
significant difference in the model's performance. In this case, models
are usually compared *sequentially*: m2 is tested against m1, m3 against
m2, m4 against m3, etc.

Two models are considered as *"non-nested"* if their predictors are
different. For instance, `model1 (y ~ x1 + x2)` and
`model2 (y ~ x3 + x4)`. In the case of non-nested models, all models are
usually compared against the same *reference* model (by default, the
first of the list).

Nesting is detected via the
[`insight::is_nested_models()`](https://easystats.github.io/insight/reference/is_nested_models.html)
function. Note that, apart from the nesting, in order for the tests to
be valid, other requirements have often to be the fulfilled. For
instance, outcome variables (the response) must be the same. You cannot
meaningfully test whether apples are significantly different from
oranges!

### Estimator of the standard deviation

The estimator is relevant when comparing regression models using
`test_likelihoodratio()`. If `estimator = "OLS"`, then it uses the same
method as `anova(..., test = "LRT")` implemented in base R, i.e.,
scaling by n-k (the unbiased OLS estimator) and using this estimator
under the alternative hypothesis. If `estimator = "ML"`, which is for
instance used by `lrtest(...)` in package **lmtest**, the scaling is
done by n (the biased ML estimator) and the estimator under the null
hypothesis. In moderately large samples, the differences should be
negligible, but it is possible that OLS would perform slightly better in
small samples with Gaussian errors. For `estimator = "REML"`, the LRT is
based on the REML-fit log-likelihoods of the models. Note that not all
types of estimators are available for all model classes.

### REML versus ML estimator

When `estimator = "ML"`, which is the default for linear mixed models
(unless they share the same fixed effects), values from information
criteria (AIC, AICc) are based on the ML-estimator, while the default
behaviour of [`AIC()`](https://rdrr.io/r/stats/AIC.html) may be
different (in particular for linear mixed models from **lme4**, which
sets `REML = TRUE`). This default in `test_likelihoodratio()`
intentional, because comparing information criteria based on REML fits
requires the same fixed effects for all models, which is often not the
case. Thus, while `anova.merMod()` automatically refits all models to
REML when performing a LRT, `test_likelihoodratio()` checks if a
comparison based on REML fits is indeed valid, and if so, uses REML as
default (else, ML is the default). Set the `estimator` argument
explicitely to override the default behaviour.

### Tests Description

- **Bayes factor for Model Comparison** - `test_bf()`: If all models
  were fit from the same data, the returned `BF` shows the Bayes Factor
  (see
  [`bayestestR::bayesfactor_models()`](https://easystats.github.io/bayestestR/reference/bayesfactor_models.html))
  for each model against the reference model (which depends on whether
  the models are nested or not). Check out [this
  vignette](https://easystats.github.io/bayestestR/articles/bayes_factors.html#bayesfactor_models)
  for more details.

- **Wald's F-Test** - `test_wald()`: The Wald test is a rough
  approximation of the Likelihood Ratio Test. However, it is more
  applicable than the LRT: you can often run a Wald test in situations
  where no other test can be run. Importantly, this test only makes
  statistical sense if the models are nested.

  Note: this test is also available in base R through the
  [`anova()`](https://rdrr.io/r/stats/anova.html) function. It returns
  an `F-value` column as a statistic and its associated p-value.

- **Likelihood Ratio Test (LRT)** - `test_likelihoodratio()`: The LRT
  tests which model is a better (more likely) explanation of the data.
  Likelihood-Ratio-Test (LRT) gives usually somewhat close results (if
  not equivalent) to the Wald test and, similarly, only makes sense for
  nested models. However, maximum likelihood tests make stronger
  assumptions than method of moments tests like the F-test, and in turn
  are more efficient. Agresti (1990) suggests that you should use the
  LRT instead of the Wald test for small sample sizes (under or
  about 30) or if the parameters are large.

  Note: for regression models, this is similar to
  `anova(..., test="LRT")` (on models) or `lmtest::lrtest(...)`,
  depending on the `estimator` argument. For **lavaan** models (SEM,
  CFA), the function calls
  [`lavaan::lavTestLRT()`](https://rdrr.io/pkg/lavaan/man/lavTestLRT.html).

  For models with transformed response variables (like `log(x)` or
  `sqrt(x)`), [`logLik()`](https://rdrr.io/r/stats/logLik.html) returns
  a wrong log-likelihood. However, `test_likelihoodratio()` calls
  [`insight::get_loglikelihood()`](https://easystats.github.io/insight/reference/get_loglikelihood.html)
  with `check_response=TRUE`, which returns a corrected log-likelihood
  value for models with transformed response variables. Furthermore,
  since the LRT only accepts nested models (i.e. models that differ in
  their fixed effects), the computed log-likelihood is always based on
  the ML estimator, not on the REML fits.

- **Vuong's Test** - `test_vuong()`: Vuong's (1989) test can be used
  both for nested and non-nested models, and actually consists of two
  tests.

  - The **Test of Distinguishability** (the `Omega2` column and its
    associated p-value) indicates whether or not the models can possibly
    be distinguished on the basis of the observed data. If its p-value
    is significant, it means the models are distinguishable.

  - The **Robust Likelihood Test** (the `LR` column and its associated
    p-value) indicates whether each model fits better than the reference
    model. If the models are nested, then the test works as a robust
    LRT. The code for this function is adapted from the **nonnest2**
    package, and all credit go to their authors.

## References

- Vuong, Q. H. (1989). Likelihood ratio tests for model selection and
  non-nested hypotheses. Econometrica, 57, 307-333.

- Merkle, E. C., You, D., & Preacher, K. (2016). Testing non-nested
  structural equation models. Psychological Methods, 21, 151-163.

## See also

[`compare_performance()`](https://easystats.github.io/performance/reference/compare_performance.md)
to compare the performance indices of many different models.

## Examples

``` r
# Nested Models
# -------------
m1 <- lm(Sepal.Length ~ Petal.Width, data = iris)
m2 <- lm(Sepal.Length ~ Petal.Width + Species, data = iris)
m3 <- lm(Sepal.Length ~ Petal.Width * Species, data = iris)

test_performance(m1, m2, m3)
#> Name | Model |    BF |   Omega2 | p (Omega2) |   LR | p (LR)
#> ------------------------------------------------------------
#> m1   |    lm |       |          |            |      |       
#> m2   |    lm | 0.007 | 9.54e-04 |      0.935 | 0.15 |  0.919
#> m3   |    lm | 0.037 |     0.02 |      0.081 | 3.41 |  0.099
#> Models were detected as nested (in terms of fixed parameters) and are compared in sequential order.

test_bf(m1, m2, m3)
#> Bayes Factors for Model Comparison
#> 
#>      Model                       BF
#> [m2] Petal.Width + Species    0.007
#> [m3] Petal.Width * Species 2.64e-04
#> 
#> * Against Denominator: [m1] Petal.Width
#> *   Bayes Factor Type: BIC approximation
test_wald(m1, m2, m3) # Equivalent to anova(m1, m2, m3)
#> Name | Model |  df | df_diff |    F |     p
#> -------------------------------------------
#> m1   |    lm | 148 |         |      |      
#> m2   |    lm | 146 |       2 | 0.08 | 0.927
#> m3   |    lm | 144 |       2 | 1.66 | 0.195
#> Models were detected as nested (in terms of fixed parameters) and are compared in sequential order.

# Equivalent to lmtest::lrtest(m1, m2, m3)
test_likelihoodratio(m1, m2, m3, estimator = "ML")
#> # Likelihood-Ratio-Test (LRT) for Model Comparison (ML-estimator)
#> 
#> Name | Model | df | df_diff | Chi2 |     p
#> ------------------------------------------
#> m1   |    lm |  3 |         |      |      
#> m2   |    lm |  5 |       2 | 0.15 | 0.926
#> m3   |    lm |  7 |       2 | 3.41 | 0.182

# Equivalent to anova(m1, m2, m3, test='LRT')
test_likelihoodratio(m1, m2, m3, estimator = "OLS")
#> # Likelihood-Ratio-Test (LRT) for Model Comparison (OLS-estimator)
#> 
#> Name | Model | df | df_diff | Chi2 |     p
#> ------------------------------------------
#> m1   |    lm |  3 |         |      |      
#> m2   |    lm |  5 |       2 | 0.15 | 0.927
#> m3   |    lm |  7 |       2 | 3.31 | 0.191

if (require("CompQuadForm")) {
  test_vuong(m1, m2, m3) # nonnest2::vuongtest(m1, m2, nested=TRUE)

  # Non-nested Models
  # -----------------
  m1 <- lm(Sepal.Length ~ Petal.Width, data = iris)
  m2 <- lm(Sepal.Length ~ Petal.Length, data = iris)
  m3 <- lm(Sepal.Length ~ Species, data = iris)

  test_performance(m1, m2, m3)
  test_bf(m1, m2, m3)
  test_vuong(m1, m2, m3) # nonnest2::vuongtest(m1, m2)
}
#> Loading required package: CompQuadForm
#> Name | Model | Omega2 | p (Omega2) |    LR | p (LR)
#> ---------------------------------------------------
#> m1   |    lm |        |            |       |       
#> m2   |    lm |   0.19 |     < .001 | -4.57 | < .001
#> m3   |    lm |   0.12 |     < .001 |  2.51 | 0.006 
#> Each model is compared to m1.

# Tweak the output
# ----------------
test_performance(m1, m2, m3, include_formula = TRUE)
#> Name |                           Model |      BF | Omega2 | p (Omega2) |    LR | p (LR)
#> ---------------------------------------------------------------------------------------
#> m1   |  lm(Sepal.Length ~ Petal.Width) |         |        |            |       |       
#> m2   | lm(Sepal.Length ~ Petal.Length) |  > 1000 |   0.19 |     < .001 | -4.57 | < .001
#> m3   |      lm(Sepal.Length ~ Species) | < 0.001 |   0.12 |     < .001 |  2.51 | 0.006 
#> Each model is compared to m1.


# SEM / CFA (lavaan objects)
# --------------------------
# Lavaan Models
if (require("lavaan")) {
  structure <- " visual  =~ x1 + x2 + x3
                 textual =~ x4 + x5 + x6
                 speed   =~ x7 + x8 + x9

                  visual ~~ textual + speed "
  m1 <- lavaan::cfa(structure, data = HolzingerSwineford1939)

  structure <- " visual  =~ x1 + x2 + x3
                 textual =~ x4 + x5 + x6
                 speed   =~ x7 + x8 + x9

                  visual ~~ 0 * textual + speed "
  m2 <- lavaan::cfa(structure, data = HolzingerSwineford1939)

  structure <- " visual  =~ x1 + x2 + x3
                 textual =~ x4 + x5 + x6
                 speed   =~ x7 + x8 + x9

                  visual ~~ 0 * textual + 0 * speed "
  m3 <- lavaan::cfa(structure, data = HolzingerSwineford1939)

  test_likelihoodratio(m1, m2, m3)

  # Different Model Types
  # ---------------------
  if (require("lme4") && require("mgcv")) {
    m1 <- lm(Sepal.Length ~ Petal.Length + Species, data = iris)
    m2 <- lmer(Sepal.Length ~ Petal.Length + (1 | Species), data = iris)
    m3 <- gam(Sepal.Length ~ s(Petal.Length, by = Species) + Species, data = iris)

    test_performance(m1, m2, m3)
  }
}
#> Loading required package: mgcv
#> This is mgcv 1.9-4. For overview type '?mgcv'.
#> 
#> Attaching package: ‘mgcv’
#> The following objects are masked from ‘package:brms’:
#> 
#>     s, t2
#> The following object is masked from ‘package:mclust’:
#> 
#>     mvn
#> Name |   Model |      BF
#> ------------------------
#> m1   |      lm |        
#> m2   | lmerMod | < 0.001
#> m3   |     gam |   0.038
#> Each model is compared to m1.
```
