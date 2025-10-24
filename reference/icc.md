# Intraclass Correlation Coefficient (ICC)

This function calculates the intraclass-correlation coefficient (ICC) -
sometimes also called *variance partition coefficient* (VPC) or
*repeatability* - for mixed effects models. The ICC can be calculated
for all models supported by
[`insight::get_variance()`](https://easystats.github.io/insight/reference/get_variance.html).
For models fitted with the **brms**-package, `icc()` might fail due to
the large variety of models and families supported by the
**brms**-package. In such cases, an alternative to the ICC is the
`variance_decomposition()`, which is based on the posterior predictive
distribution (see 'Details').

## Usage

``` r
icc(
  model,
  by_group = FALSE,
  tolerance = 1e-05,
  ci = NULL,
  iterations = 100,
  ci_method = NULL,
  null_model = NULL,
  approximation = "lognormal",
  model_component = NULL,
  verbose = TRUE,
  ...
)

variance_decomposition(model, re_formula = NULL, robust = TRUE, ci = 0.95, ...)
```

## Arguments

- model:

  A (Bayesian) mixed effects model.

- by_group:

  Logical, if `TRUE`, `icc()` returns the variance components for each
  random-effects level (if there are multiple levels). See 'Details'.

- tolerance:

  Tolerance for singularity check of random effects, to decide whether
  to compute random effect variances or not. Indicates up to which value
  the convergence result is accepted. The larger tolerance is, the
  stricter the test will be. See
  [`performance::check_singularity()`](https://easystats.github.io/performance/reference/check_singularity.md).

- ci:

  Confidence resp. credible interval level. For `icc()`,
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
  bootstrapped ICC, R2, RMSE etc.; for `variance_decomposition()`,
  arguments are passed down to
  [`brms::posterior_predict()`](https://mc-stan.org/rstantools/reference/posterior_predict.html).

- re_formula:

  Formula containing group-level effects to be considered in the
  prediction. If `NULL` (default), include all group-level effects.
  Else, for instance for nested models, name a specific group-level
  effect to calculate the variance decomposition for this group-level.
  See 'Details' and
  [`?brms::posterior_predict`](https://mc-stan.org/rstantools/reference/posterior_predict.html).

- robust:

  Logical, if `TRUE`, the median instead of mean is used to calculate
  the central tendency of the variances.

## Value

A list with two values, the adjusted ICC and the unadjusted ICC. For
`variance_decomposition()`, a list with two values, the decomposed ICC
as well as the credible intervals for this ICC.

## Details

### Interpretation

The ICC can be interpreted as "the proportion of the variance explained
by the grouping structure in the population". The grouping structure
entails that measurements are organized into groups (e.g., test scores
in a school can be grouped by classroom if there are multiple classrooms
and each classroom was administered the same test) and ICC indexes how
strongly measurements in the same group resemble each other. This index
goes from 0, if the grouping conveys no information, to 1, if all
observations in a group are identical (*Gelman and Hill, 2007, p. 258*).
In other word, the ICC - sometimes conceptualized as the measurement
repeatability - "can also be interpreted as the expected correlation
between two randomly drawn units that are in the same group" *(Hox 2010:
15)*, although this definition might not apply to mixed models with more
complex random effects structures. The ICC can help determine whether a
mixed model is even necessary: an ICC of zero (or very close to zero)
means the observations within clusters are no more similar than
observations from different clusters, and setting it as a random factor
might not be necessary.

### Difference with R2

The coefficient of determination R2 (that can be computed with
[`r2()`](https://easystats.github.io/performance/reference/r2.md))
quantifies the proportion of variance explained by a statistical model,
but its definition in mixed model is complex (hence, different methods
to compute a proxy exist). ICC is related to R2 because they are both
ratios of variance components. More precisely, R2 is the proportion of
the explained variance (of the full model), while the ICC is the
proportion of explained variance that can be attributed to the random
effects. In simple cases, the ICC corresponds to the difference between
the *conditional R2* and the *marginal R2* (see
[`r2_nakagawa()`](https://easystats.github.io/performance/reference/r2_nakagawa.md)).

### Calculation

The ICC is calculated by dividing the random effect variance, σ²_(i), by
the total variance, i.e. the sum of the random effect variance and the
residual variance, σ²_(ε).

### Adjusted and unadjusted ICC

`icc()` calculates an adjusted and an unadjusted ICC, which both take
all sources of uncertainty (i.e. of *all random effects*) into account.
While the *adjusted ICC* only relates to the random effects, the
*unadjusted ICC* also takes the fixed effects variances into account,
more precisely, the fixed effects variance is added to the denominator
of the formula to calculate the ICC (see *Nakagawa et al. 2017*).
Typically, the *adjusted* ICC is of interest when the analysis of random
effects is of interest. `icc()` returns a meaningful ICC also for more
complex random effects structures, like models with random slopes or
nested design (more than two levels) and is applicable for models with
other distributions than Gaussian. For more details on the computation
of the variances, see
[`?insight::get_variance`](https://easystats.github.io/insight/reference/get_variance.html).

### ICC for unconditional and conditional models

Usually, the ICC is calculated for the null model ("unconditional
model"). However, according to *Raudenbush and Bryk (2002)* or
*Rabe-Hesketh and Skrondal (2012)* it is also feasible to compute the
ICC for full models with covariates ("conditional models") and compare
how much, e.g., a level-2 variable explains the portion of variation in
the grouping structure (random intercept).

### ICC for specific group-levels

The proportion of variance for specific levels related to the overall
model can be computed by setting `by_group = TRUE`. The reported ICC is
the variance for each (random effect) group compared to the total
variance of the model. For mixed models with a simple random intercept,
this is identical to the classical (adjusted) ICC.

### Variance decomposition for brms-models

If `model` is of class `brmsfit`, `icc()` might fail due to the large
variety of models and families supported by the **brms** package. In
such cases, `variance_decomposition()` is an alternative ICC measure.
The function calculates a variance decomposition based on the posterior
predictive distribution. In this case, first, the draws from the
posterior predictive distribution *not conditioned* on group-level terms
(`posterior_predict(..., re_formula = NA)`) are calculated as well as
draws from this distribution *conditioned* on *all random effects* (by
default, unless specified else in `re_formula`) are taken. Then, second,
the variances for each of these draws are calculated. The "ICC" is then
the ratio between these two variances. This is the recommended way to
analyse random-effect-variances for non-Gaussian models. It is then
possible to compare variances across models, also by specifying
different group-level terms via the `re_formula`-argument.

Sometimes, when the variance of the posterior predictive distribution is
very large, the variance ratio in the output makes no sense, e.g.
because it is negative. In such cases, it might help to use
`robust = TRUE`.

## Supported models and model families

The single variance components that are required to calculate the
marginal and conditional r-squared values are calculated using the
[`insight::get_variance()`](https://easystats.github.io/insight/reference/get_variance.html)
function. The results are validated against the solutions provided by
*Nakagawa et al. (2017)*, in particular examples shown in the Supplement
2 of the paper. Other model families are validated against results from
the **MuMIn** package. This means that the r-squared values returned by
[`r2_nakagawa()`](https://easystats.github.io/performance/reference/r2_nakagawa.md)
should be accurate and reliable for following mixed models or model
families:

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

- Nakagawa, S., Johnson, P. C. D., and Schielzeth, H. (2017). The
  coefficient of determination R2 and intra-class correlation
  coefficient from generalized linear mixed-effects models revisited and
  expanded. Journal of The Royal Society Interface, 14(134), 20170213.

- Rabe-Hesketh, S., and Skrondal, A. (2012). Multilevel and longitudinal
  modeling using Stata (3rd ed). College Station, Tex: Stata Press
  Publication.

- Raudenbush, S. W., and Bryk, A. S. (2002). Hierarchical linear models:
  applications and data analysis methods (2nd ed). Thousand Oaks: Sage
  Publications.

## Examples

``` r
model <- lme4::lmer(Sepal.Length ~ Petal.Length + (1 | Species), data = iris)
icc(model)
#> # Intraclass Correlation Coefficient
#> 
#>     Adjusted ICC: 0.910
#>   Unadjusted ICC: 0.311

# ICC for specific group-levels
data(sleepstudy, package = "lme4")
set.seed(12345)
sleepstudy$grp <- sample(1:5, size = 180, replace = TRUE)
sleepstudy$subgrp <- NA
for (i in 1:5) {
  filter_group <- sleepstudy$grp == i
  sleepstudy$subgrp[filter_group] <-
    sample(1:30, size = sum(filter_group), replace = TRUE)
}
model <- lme4::lmer(
  Reaction ~ Days + (1 | grp / subgrp) + (1 | Subject),
  data = sleepstudy
)
icc(model, by_group = TRUE)
#> # ICC by Group
#> 
#> Group      |   ICC
#> ------------------
#> subgrp:grp | 0.017
#> Subject    | 0.589
#> grp        | 0.001
```
