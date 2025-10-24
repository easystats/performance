# Visual check of model assumptions

Visual check of various model assumptions (normality of residuals,
normality of random effects, linear relationship, homogeneity of
variance, multicollinearity).

If `check_model()` doesn't work as expected, try setting
`verbose = TRUE` to get hints about possible problems.

## Usage

``` r
check_model(x, ...)

# Default S3 method
check_model(
  x,
  panel = TRUE,
  check = "all",
  detrend = TRUE,
  bandwidth = "nrd",
  type = "density",
  residual_type = NULL,
  show_dots = NULL,
  size_dot = 2,
  size_line = 0.8,
  size_title = 12,
  size_axis_title = base_size,
  base_size = 10,
  alpha = 0.2,
  alpha_dot = 0.8,
  colors = c("#3aaf85", "#1b6ca8", "#cd201f"),
  theme = "see::theme_lucid",
  verbose = FALSE,
  ...
)
```

## Arguments

- x:

  A model object.

- ...:

  Arguments passed down to the individual check functions, especially to
  [`check_predictions()`](https://easystats.github.io/performance/reference/check_predictions.md)
  and
  [`binned_residuals()`](https://easystats.github.io/performance/reference/binned_residuals.md).

- panel:

  Logical, if `TRUE`, plots are arranged as panels; else, single plots
  for each diagnostic are returned.

- check:

  Character vector, indicating which checks for should be performed and
  plotted. May be one or more of `"all"`, `"vif"`, `"qq"`,
  `"normality"`, `"linearity"`, `"ncv"`, `"homogeneity"`, `"outliers"`,
  `"reqq"`, `"pp_check"`, `"binned_residuals"` or `"overdispersion"`.
  Note that not all check apply to all type of models (see 'Details').
  `"reqq"` is a QQ-plot for random effects and only available for mixed
  models. `"ncv"` is an alias for `"linearity"`, and checks for
  non-constant variance, i.e. for heteroscedasticity, as well as the
  linear relationship. By default, all possible checks are performed and
  plotted.

- detrend:

  Logical. Should Q-Q/P-P plots be detrended? Defaults to `TRUE` for
  linear models or when `residual_type = "normal"`. Defaults to `FALSE`
  for QQ plots based on simulated residuals (i.e. when
  `residual_type = "simulated"`).

- bandwidth:

  A character string indicating the smoothing bandwidth to be used.
  Unlike [`stats::density()`](https://rdrr.io/r/stats/density.html),
  which used `"nrd0"` as default, the default used here is `"nrd"`
  (which seems to give more plausible results for non-Gaussian models).
  When problems with plotting occur, try to change to a different value.

- type:

  Plot type for the posterior predictive checks plot. Can be
  `"density"`, `"discrete_dots"`, `"discrete_interval"` or
  `"discrete_both"` (the `discrete_*` options are appropriate for models
  with discrete - binary, integer or ordinal etc. - outcomes).

- residual_type:

  Character, indicating the type of residuals to be used. For
  non-Gaussian models, the default is `"simulated"`, which uses
  simulated residuals. These are based on
  [`simulate_residuals()`](https://easystats.github.io/performance/reference/simulate_residuals.md)
  and thus uses the **DHARMa** package to return randomized quantile
  residuals. For Gaussian models, the default is `"normal"`, which uses
  the default residuals from the model. Setting
  `residual_type = "normal"` for non-Gaussian models will use a
  half-normal Q-Q plot of the absolute value of the standardized
  deviance residuals.

- show_dots:

  Logical, if `TRUE`, will show data points in the plot. Set to `FALSE`
  for models with many observations, if generating the plot is too
  time-consuming. By default, `show_dots = NULL`. In this case
  `check_model()` tries to guess whether performance will be poor due to
  a very large model and thus automatically shows or hides dots.

- size_dot, size_line:

  Size of line and dot-geoms.

- base_size, size_title, size_axis_title:

  Base font size for axis and plot titles.

- alpha, alpha_dot:

  The alpha level of the confidence bands and dot-geoms. Scalar from 0
  to 1.

- colors:

  Character vector with color codes (hex-format). Must be of length 3.
  First color is usually used for reference lines, second color for
  dots, and third color for outliers or extreme values.

- theme:

  String, indicating the name of the plot-theme. Must be in the format
  `"package::theme_name"` (e.g. `"ggplot2::theme_minimal"`).

- verbose:

  If `FALSE` (default), suppress most warning messages.

## Value

The data frame that is used for plotting.

## Details

For Bayesian models from packages **rstanarm** or **brms**, models will
be "converted" to their frequentist counterpart, using
[[`bayestestR::bayesian_as_frequentist`](https://easystats.github.io/bayestestR/reference/convert_bayesian_as_frequentist.html)](https://easystats.github.io/bayestestR/reference/convert_bayesian_as_frequentist.html).
A more advanced model-check for Bayesian models will be implemented at a
later stage.

See also the related
[vignette](https://easystats.github.io/performance/articles/check_model.html).

## Note

This function just prepares the data for plotting. To create the plots,
**see** needs to be installed. Furthermore, this function suppresses all
possible warnings. In case you observe suspicious plots, please refer to
the dedicated functions (like
[`check_collinearity()`](https://easystats.github.io/performance/reference/check_collinearity.md),
[`check_normality()`](https://easystats.github.io/performance/reference/check_normality.md)
etc.) to get informative messages and warnings.

## Posterior Predictive Checks

Posterior predictive checks can be used to look for systematic
discrepancies between real and simulated data. It helps to see whether
the type of model (distributional family) fits well to the data. See
[`check_predictions()`](https://easystats.github.io/performance/reference/check_predictions.md)
for further details.

## Linearity Assumption

The plot **Linearity** checks the assumption of linear relationship.
However, the spread of dots also indicate possible heteroscedasticity
(i.e. non-constant variance, hence, the alias `"ncv"` for this plot),
thus it shows if residuals have non-linear patterns. This plot helps to
see whether predictors may have a non-linear relationship with the
outcome, in which case the reference line may roughly indicate that
relationship. A straight and horizontal line indicates that the model
specification seems to be ok. But for instance, if the line would be
U-shaped, some of the predictors probably should better be modeled as
quadratic term. See
[`check_heteroscedasticity()`](https://easystats.github.io/performance/reference/check_heteroscedasticity.md)
for further details.

**Some caution is needed** when interpreting these plots. Although these
plots are helpful to check model assumptions, they do not necessarily
indicate so-called "lack of fit", e.g. missed non-linear relationships
or interactions. Thus, it is always recommended to also look at [effect
plots, including partial
residuals](https://strengejacke.github.io/ggeffects/articles/introduction_partial_residuals.html).

## Homogeneity of Variance

This plot checks the assumption of equal variance (homoscedasticity).
The desired pattern would be that dots spread equally above and below a
straight, horizontal line and show no apparent deviation.

## Influential Observations

This plot is used to identify influential observations. If any points in
this plot fall outside of Cook’s distance (the dashed lines) then it is
considered an influential observation. See
[`check_outliers()`](https://easystats.github.io/performance/reference/check_outliers.md)
for further details.

## Multicollinearity

This plot checks for potential collinearity among predictors. In a
nutshell, multicollinearity means that once you know the effect of one
predictor, the value of knowing the other predictor is rather low.
Multicollinearity might arise when a third, unobserved variable has a
causal effect on each of the two predictors that are associated with the
outcome. In such cases, the actual relationship that matters would be
the association between the unobserved variable and the outcome. See
[`check_collinearity()`](https://easystats.github.io/performance/reference/check_collinearity.md)
for further details.

## Normality of Residuals

This plot is used to determine if the residuals of the regression model
are normally distributed. Usually, dots should fall along the line. If
there is some deviation (mostly at the tails), this indicates that the
model doesn't predict the outcome well for that range that shows larger
deviations from the line. For generalized linear models and when
`residual_type = "normal"`, a half-normal Q-Q plot of the absolute value
of the standardized deviance residuals is shown, however, the
interpretation of the plot remains the same. See
[`check_normality()`](https://easystats.github.io/performance/reference/check_normality.md)
for further details. Usually, for generalized linear (mixed) models, a
test comparing simulated quantile residuals against the uniform
distribution is conducted (see next section).

## Distribution of Simulated Quantile Residuals

Fore non-Gaussian models, when `residual_type = "simulated"` (the
default for generalized linear (mixed) models), residuals are not
expected to be normally distributed. In this case, we generate simulated
quantile residuals to compare whether observed response values deviate
from model expectations. Simulated quantile residuals are generated by
simulating a series of values from a fitted model for each case,
comparing the observed response values to these simulations, and
computing the empirical quantile of the observed value in the
distribution of simulated values. When the model is correctly-specified,
these quantile residuals will follow a *uniform* (flat) distribution.
The Q-Q plot compares the simulated quantile residuals against a uniform
distribution. The plot is interpreted in the same way as for a
normal-distribution Q-Q plot in linear regression. See
[`simulate_residuals()`](https://easystats.github.io/performance/reference/simulate_residuals.md)
and
[`check_residuals()`](https://easystats.github.io/performance/reference/check_residuals.md)
for further details.

## Overdispersion

For count models, an *overdispersion plot* is shown. Overdispersion
occurs when the observed variance is higher than the variance of a
theoretical model. For Poisson models, variance increases with the mean
and, therefore, variance usually (roughly) equals the mean value. If the
variance is much higher, the data are "overdispersed". See
[`check_overdispersion()`](https://easystats.github.io/performance/reference/check_overdispersion.md)
for further details.

## Binned Residuals

For models from binomial families, a *binned residuals plot* is shown.
Binned residual plots are achieved by cutting the the data into bins and
then plotting the average residual versus the average fitted value for
each bin. If the model were true, one would expect about 95% of the
residuals to fall inside the error bounds. See
[`binned_residuals()`](https://easystats.github.io/performance/reference/binned_residuals.md)
for further details.

## Residuals for (Generalized) Linear Models

Plots that check the homogeneity of variance use standardized Pearson's
residuals for generalized linear models, and standardized residuals for
linear models. The plots for the normality of residuals (with overlayed
normal curve) and for the linearity assumption use the default residuals
for `lm` and `glm` (which are deviance residuals for `glm`). The Q-Q
plots use simulated quantile residuals (see
[`simulate_residuals()`](https://easystats.github.io/performance/reference/simulate_residuals.md))
for non-Gaussian models and standardized residuals for linear models.

## Troubleshooting

For models with many observations, or for more complex models in
general, generating the plot might become very slow. One reason might be
that the underlying graphic engine becomes slow for plotting many data
points. In such cases, setting the argument `show_dots = FALSE` might
help. Furthermore, look at the `check` argument and see if some of the
model checks could be skipped, which also increases performance.

If `check_model()` doesn't work as expected, try setting
`verbose = TRUE` to get hints about possible problems.

## See also

Other functions to check model assumptions and and assess model quality:
[`check_autocorrelation()`](https://easystats.github.io/performance/reference/check_autocorrelation.md),
[`check_collinearity()`](https://easystats.github.io/performance/reference/check_collinearity.md),
[`check_convergence()`](https://easystats.github.io/performance/reference/check_convergence.md),
[`check_heteroscedasticity()`](https://easystats.github.io/performance/reference/check_heteroscedasticity.md),
[`check_homogeneity()`](https://easystats.github.io/performance/reference/check_homogeneity.md),
[`check_outliers()`](https://easystats.github.io/performance/reference/check_outliers.md),
[`check_overdispersion()`](https://easystats.github.io/performance/reference/check_overdispersion.md),
[`check_predictions()`](https://easystats.github.io/performance/reference/check_predictions.md),
[`check_singularity()`](https://easystats.github.io/performance/reference/check_singularity.md),
[`check_zeroinflation()`](https://easystats.github.io/performance/reference/check_zeroinflation.md)

## Examples

``` r
# \donttest{
m <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
check_model(m)


data(sleepstudy, package = "lme4")
m <- lme4::lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
check_model(m, panel = FALSE)
# }
```
