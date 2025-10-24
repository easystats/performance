# Performance of Meta-Analysis Models

Compute indices of model performance for meta-analysis model from the
**metafor** package.

## Usage

``` r
# S3 method for class 'rma'
model_performance(
  model,
  metrics = "all",
  estimator = "ML",
  verbose = TRUE,
  ...
)
```

## Arguments

- model:

  A `rma` object as returned by
  [`metafor::rma()`](https://wviechtb.github.io/metafor/reference/rma.uni.html).

- metrics:

  Can be `"all"` or a character vector of metrics to be computed (some
  of
  `c("AIC", "BIC", "I2", "H2", "TAU2", "R2", "CochransQ", "QE", "Omnibus", "QM")`).

- estimator:

  Only for linear models. Corresponds to the different estimators for
  the standard deviation of the errors. If `estimator = "ML"` (default,
  except for
  [`performance_aic()`](https://easystats.github.io/performance/reference/performance_aicc.md)
  when the model object is of class `lmerMod`), the scaling is done by
  `n` (the biased ML estimator), which is then equivalent to using
  `AIC(logLik())`. Setting it to `"REML"` will give the same results as
  `AIC(logLik(..., REML = TRUE))`.

- verbose:

  Toggle off warnings.

- ...:

  Arguments passed to or from other methods.

## Value

A data frame (with one row) and one column per "index" (see `metrics`).

## Details

### Indices of fit

- **AIC** Akaike's Information Criterion, see
  [`?stats::AIC`](https://rdrr.io/r/stats/AIC.html)

- **BIC** Bayesian Information Criterion, see
  [`?stats::BIC`](https://rdrr.io/r/stats/AIC.html)

- **I2**: For a random effects model, `I2` estimates (in percent) how
  much of the total variability in the effect size estimates can be
  attributed to heterogeneity among the true effects. For a
  mixed-effects model, `I2` estimates how much of the unaccounted
  variability can be attributed to residual heterogeneity.

- **H2**: For a random-effects model, `H2` estimates the ratio of the
  total amount of variability in the effect size estimates to the amount
  of sampling variability. For a mixed-effects model, `H2` estimates the
  ratio of the unaccounted variability in the effect size estimates to
  the amount of sampling variability.

- **TAU2**: The amount of (residual) heterogeneity in the random or
  mixed effects model.

- **CochransQ (QE)**: Test for (residual) Heterogeneity. Without
  moderators in the model, this is simply Cochran's *Q*-test.

- **Omnibus (QM)**: Omnibus test of parameters.

- **R2**: Pseudo-R2-statistic, which indicates the amount of
  heterogeneity accounted for by the moderators included in a
  fixed-effects model.

See the documentation for
[`?metafor::fitstats`](https://wviechtb.github.io/metafor/reference/fitstats.html).

## Examples

``` r
data(dat.bcg, package = "metadat")
dat <- metafor::escalc(
  measure = "RR",
  ai = tpos,
  bi = tneg,
  ci = cpos,
  di = cneg,
  data = dat.bcg
)
model <- metafor::rma(yi, vi, data = dat, method = "REML")
model_performance(model)
#> # Indices of model performance
#> 
#> AIC  |  BIC |    I2 |     H2 |  TAU2 | CochransQ | p (CochransQ) | df | Omnibus | p (Omnibus)
#> ---------------------------------------------------------------------------------------------
#> 29.4 | 30.5 | 0.922 | 12.856 | 0.313 |   152.233 |        < .001 | 12 |  15.796 |      < .001
```
