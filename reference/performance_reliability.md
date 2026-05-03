# Random Effects Reliability

These functions provide information about the reliability of group-level
estimates (i.e., random effects) in mixed models. They are useful to
assess whether the predictors yield consistent group-level variability.
"Group-level" can refer, for instance, to different participants in a
study, and the predictors to the effect of some experimental condition.

The conceptually related functions are implemented,
`performance_reliability()`, based on Rouder & Mehrvarz (2024) that uses
estimated model variances, and `performance_dvour()` (d-vour), which
corresponds to the Variability-Over-Uncertainty Ratio ("vour") between
random effects coefficient variability and their associated uncertainty.

**Note**: `performance_reliability()` requires to recompute the model to
estimate some of the variances of interest, which does not make it very
usable with Bayesian models. Please get in touch if you have would like
to help addressing this.

## Usage

``` r
performance_reliability(x, ...)

performance_dvour(x, ...)
```

## Arguments

- x:

  A model object.

- ...:

  Currently not used.

## Details

### Reliability (Signal-to-Noise Ratio)

`performance_reliability()` estimates the reliability of random effects
(intercepts and slopes) in mixed-effects models using variance
decomposition. This method follows the **hierarchical modeling**
framework of Rouder & Mehrvarz (2024), defining reliability as the
**signal-to-noise variance ratio**:

\$\$\gamma^2 = \frac{\sigma_B^2}{\sigma_B^2 + \sigma_W^2}\$\$

where:

- \\\sigma_B^2\\ is the **between-subject variance** (i.e., variability
  across groups).

- \\\sigma_W^2\\ is the **within-subject variance** (i.e., trial-level
  measurement noise).

This metric quantifies **how much observed variability is due to actual
differences between groups**, rather than measurement error or
within-group fluctuations.

To account for **trial count (\\L\\)**, reliability is adjusted
following:

\$\$E(r) = \frac{\gamma^2}{\gamma^2 + 1/L}\$\$

where \\L\\ is the number of **observations per random effect level**
(note that Rouder (2024) recommends 2/L to adjust for contrast effects).

### Variability-Over-Uncertainty Ratio (d-vour)

`performance_dvour()` computes an alternative reliability measure
corresponding to the normalized **ratio of observed variability to
uncertainty in random effect estimates**. This is defined as:

\$\$\text{D-vour} = \frac{\sigma_B^2}{\sigma_B^2 +
\mu\_{\text{SE}}^2}\$\$

where:

- \\\sigma_B^2\\ is the **between-group variability** (computed as the
  SD of the random effect estimates).

- \\\mu\_{\text{SE}}^2\\ is the **mean squared uncertainty** in random
  effect estimates (i.e., the average uncertainty).

#### Interpretation:

- **D-vour \> 0.75**: Strong group-level effects (between-group variance
  is at least 3 times greater than uncertainty).

- **D-vour ~ 0.5**: Within-group and between-group variability are
  similar; random effect estimates should be used with caution.

- **D-vour \< 0.5**: Measurement noise dominates; random effect
  estimates are probably unreliable.

While d-vour shares some similarity to Rouder's Reliability, it does not
explicitly model within-group trial-level noise and is only based on the
random effect estimates, and can thus be not accurate when there is not
a lot of random factor groups (the reliability of this index - the
meta-reliability - depends on the number of groups).

## References

- Rouder, J. N., Pena, A. L., Mehrvarz, M., & Vandekerckhove, J. (2024).
  On Cronbach’s merger: Why experiments may not be suitable for
  measuring individual differences.

- Rouder, J. N., & Mehrvarz, M. (2024). Hierarchical-model insights for
  planning and interpreting individual-difference studies of cognitive
  abilities. Current Directions in Psychological Science, 33(2),
  128-135.

- Williams, D. R., Mulder, J., Rouder, J. N., & Rast, P. (2021). Beneath
  the surface: Unearthing within-person variability and mean relations
  with Bayesian mixed models. Psychological methods, 26(1), 74.

- Williams, D. R., Martin, S. R., DeBolt, M., Oakes, L., & Rast, P.
  (2020). A fine-tooth comb for measurement reliability: Predicting true
  score and error variance in hierarchical models.

## Examples

``` r
url <- "https://raw.githubusercontent.com/easystats/circus/refs/heads/main/data/illusiongame.csv"
df <- read.csv(url)

m <- lme4::lmer(RT ~ (1 | Participant), data = df)
performance_reliability(m)
#>         Group   Parameter Reliability
#> 1 Participant (Intercept)    0.155448
performance_dvour(m)
#>         Group   Parameter    D_vour
#> 1 Participant (Intercept) 0.9781019

m <- glmmTMB::glmmTMB(RT ~ (1 | Participant), data = df)
performance_reliability(m)
#>         Group   Parameter Reliability
#> 1 Participant (Intercept)   0.1528589
performance_dvour(m)
#>         Group   Parameter    D_vour
#> 1 Participant (Intercept) 0.9603575

m <- lme4::lmer(RT ~ (1 | Participant) + (1 | Trial), data = df)
performance_reliability(m)
#>         Group   Parameter Reliability
#> 1       Trial (Intercept) 0.005897166
#> 2 Participant (Intercept) 0.156391605
performance_dvour(m)
#>         Group   Parameter    D_vour
#> 1 Participant (Intercept) 0.9777044
#> 2       Trial (Intercept) 0.5664226

m <- glmmTMB::glmmTMB(RT ~ (1 | Participant) + (1 | Trial), data = df)
performance_reliability(m)
#>         Group   Parameter Reliability
#> 1 Participant (Intercept)  0.15386342
#> 2       Trial (Intercept)  0.00588784
performance_dvour(m)
#> Cannot extract confidence intervals for random variance parameters from
#>   models with more than one grouping factor.
#>         Group   Parameter    D_vour
#> 1 Participant (Intercept) 0.9604671
#> 2       Trial (Intercept) 0.5602238

# \donttest{
m <- lme4::lmer(
  RT ~ Illusion_Difference + (Illusion_Difference | Participant) + (1 | Trial),
  data = df
)
performance_reliability(m)
#>         Group           Parameter Reliability
#> 1       Trial         (Intercept) 0.005869221
#> 3 Participant         (Intercept) 0.221961581
#> 4 Participant Illusion_Difference 0.170477785
performance_dvour(m)
#>         Group           Parameter    D_vour
#> 1 Participant         (Intercept) 0.9673865
#> 2 Participant Illusion_Difference 0.7655642
#> 3       Trial         (Intercept) 0.5648709

m <- glmmTMB::glmmTMB(
  RT ~ Illusion_Difference + (Illusion_Difference | Participant) + (1 | Trial),
  data = df
)
performance_reliability(m)
#>         Group           Parameter Reliability
#> 1 Participant         (Intercept) 0.218590368
#> 2 Participant Illusion_Difference 0.166705433
#> 3       Trial         (Intercept) 0.005854992
performance_dvour(m)
#> Cannot extract confidence intervals for random variance parameters from
#>   models with more than one grouping factor.
#>         Group           Parameter    D_vour
#> 1 Participant         (Intercept) 0.9500691
#> 2 Participant Illusion_Difference 0.7484011
#> 3       Trial         (Intercept) 0.5584429
# }
```
