# Nakagawa's R2 for mixed models

Compute the *marginal* and *conditional* r-squared value for mixed
effects models with complex random effects structures.

## Usage

``` r
r2_nakagawa(
  model,
  by_group = FALSE,
  tolerance = 1e-08,
  ci = NULL,
  iterations = 100,
  ci_method = NULL,
  null_model = NULL,
  approximation = "lognormal",
  model_component = NULL,
  verbose = TRUE,
  ...
)
```

## Arguments

- model:

  A mixed effects model.

- by_group:

  Logical, if `TRUE`, returns the explained variance at different levels
  (if there are multiple levels). This is essentially similar to the
  variance reduction approach by *Hox (2010), pp. 69-78*.

- tolerance:

  Tolerance for singularity check of random effects, to decide whether
  to compute random effect variances for the conditional r-squared or
  not. Indicates up to which value the convergence result is accepted.
  When `r2_nakagawa()` returns a warning, stating that random effect
  variances can't be computed (and thus, the conditional r-squared is
  `NA`), decrease the tolerance-level. See also
  [`check_singularity()`](https://easystats.github.io/performance/reference/check_singularity.md).

- ci:

  Confidence resp. credible interval level. For
  [`icc()`](https://easystats.github.io/performance/reference/icc.md),
  [`r2()`](https://easystats.github.io/performance/reference/r2.md), and
  [`rmse()`](https://easystats.github.io/performance/reference/performance_rmse.md),
  confidence intervals are based on bootstrapped samples from the ICC,
  R2 or RMSE value. See `iterations`.

- iterations:

  Number of bootstrap-replicates when computing confidence intervals for
  the ICC, R2, RMSE etc.

- ci_method:

  Character string, indicating the bootstrap-method. Should be `NULL`
  (default), in which case
  [`lme4::bootMer()`](https://rdrr.io/pkg/lme4/man/bootMer.html) is used
  for bootstrapped confidence intervals. However, if bootstrapped
  intervals cannot be calculated this way, try `ci_method = "boot"`,
  which falls back to
  [`boot::boot()`](https://rdrr.io/pkg/boot/man/boot.html). This may
  successfully return bootstrapped confidence intervals, but
  bootstrapped samples may not be appropriate for the multilevel
  structure of the model. There is also an option
  `ci_method = "analytical"`, which tries to calculate analytical
  confidence assuming a chi-squared distribution. However, these
  intervals are rather inaccurate and often too narrow. It is
  recommended to calculate bootstrapped confidence intervals for mixed
  models.

- null_model:

  Optional, a null model to compute the random effect variances, which
  is passed to
  [`insight::get_variance()`](https://easystats.github.io/insight/reference/get_variance.html).
  Usually only required if calculation of r-squared or ICC fails when
  `null_model` is not specified. If calculating the null model takes
  longer and you already have fit the null model, you can pass it here,
  too, to speed up the process.

- approximation:

  Character string, indicating the approximation method for the
  distribution-specific (observation level, or residual) variance. Only
  applies to non-Gaussian models. Can be `"lognormal"` (default),
  `"delta"` or `"trigamma"`. For binomial models, the default is the
  *theoretical* distribution specific variance, however, it can also be
  `"observation_level"`. See *Nakagawa et al. 2017*, in particular
  supplement 2, for details.

- model_component:

  For models that can have a zero-inflation component, specify for which
  component variances should be returned. If `NULL` or `"full"` (the
  default), both the conditional and the zero-inflation component are
  taken into account. If `"conditional"`, only the conditional component
  is considered.

- verbose:

  Toggle warnings and messages.

- ...:

  Arguments passed down to
  [`lme4::bootMer()`](https://rdrr.io/pkg/lme4/man/bootMer.html) or
  [`boot::boot()`](https://rdrr.io/pkg/boot/man/boot.html) for
  bootstrapped ICC, R2, RMSE etc.; for
  [`variance_decomposition()`](https://easystats.github.io/performance/reference/icc.md),
  arguments are passed down to
  [`brms::posterior_predict()`](https://mc-stan.org/rstantools/reference/posterior_predict.html).

## Value

A list with the conditional and marginal R2 values.

## Details

Marginal and conditional r-squared values for mixed models are
calculated based on *Nakagawa et al. (2017)*. For more details on the
computation of the variances, see
[`insight::get_variance()`](https://easystats.github.io/insight/reference/get_variance.html).
The random effect variances are actually the mean random effect
variances, thus the r-squared value is also appropriate for mixed models
with random slopes or nested random effects (see *Johnson, 2014*).

- **Conditional R2**: takes both the fixed and random effects into
  account.

- **Marginal R2**: considers only the variance of the fixed effects.

The contribution of random effects can be deduced by subtracting the
marginal R2 from the conditional R2 or by computing the
[`icc()`](https://easystats.github.io/performance/reference/icc.md).

## Supported models and model families

The single variance components that are required to calculate the
marginal and conditional r-squared values are calculated using the
[`insight::get_variance()`](https://easystats.github.io/insight/reference/get_variance.html)
function. The results are validated against the solutions provided by
*Nakagawa et al. (2017)*, in particular examples shown in the Supplement
2 of the paper. Other model families are validated against results from
the **MuMIn** package. This means that the r-squared values returned by
`r2_nakagawa()` should be accurate and reliable for following mixed
models or model families:

- Bernoulli (logistic) regression

- Binomial regression (with other than binary outcomes)

- Poisson and Quasi-Poisson regression

- Negative binomial regression (including nbinom1, nbinom2 and nbinom12
  families)

- Gaussian regression (linear models)

- Gamma regression

- Tweedie regression

- Beta regression

- Ordered beta regression

Following model families are not yet validated, but should work:

- Zero-inflated and hurdle models

- Beta-binomial regression

- Compound Poisson regression

- Generalized Poisson regression

- Log-normal regression

- Skew-normal regression

Extracting variance components for models with zero-inflation part is
not straightforward, because it is not definitely clear how the
distribution-specific variance should be calculated. Therefore, it is
recommended to carefully inspect the results, and probably validate
against other models, e.g. Bayesian models (although results may be only
roughly comparable).

Log-normal regressions (e.g.
[`lognormal()`](https://rdrr.io/pkg/glmmTMB/man/nbinom2.html) family in
**glmmTMB** or `gaussian("log")`) often have a very low fixed effects
variance (if they were calculated as suggested by *Nakagawa et al.
2017*). This results in very low ICC or r-squared values, which may not
be meaningful.

## References

- Hox, J. J. (2010). Multilevel analysis: techniques and applications
  (2nd ed). New York: Routledge.

- Johnson, P. C. D. (2014). Extension of Nakagawa and Schielzeth’s R2
  GLMM to random slopes models. Methods in Ecology and Evolution, 5(9),
  944–946.
  [doi:10.1111/2041-210X.12225](https://doi.org/10.1111/2041-210X.12225)

- Nakagawa, S., and Schielzeth, H. (2013). A general and simple method
  for obtaining R2 from generalized linear mixed-effects models. Methods
  in Ecology and Evolution, 4(2), 133–142.
  [doi:10.1111/j.2041-210x.2012.00261.x](https://doi.org/10.1111/j.2041-210x.2012.00261.x)

- Nakagawa, S., Johnson, P. C. D., and Schielzeth, H. (2017). The
  coefficient of determination R2 and intra-class correlation
  coefficient from generalized linear mixed-effects models revisited and
  expanded. Journal of The Royal Society Interface, 14(134), 20170213.

## Examples

``` r
model <- lme4::lmer(Sepal.Length ~ Petal.Length + (1 | Species), data = iris)
r2_nakagawa(model)
#> # R2 for Mixed Models
#> 
#>   Conditional R2: 0.969
#>      Marginal R2: 0.658
r2_nakagawa(model, by_group = TRUE)
#> # Explained Variance by Level
#> 
#> Level   |     R2
#> ----------------
#> Level 1 |  0.569
#> Species | -0.853
#> 
```
