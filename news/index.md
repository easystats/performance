# Changelog

## performance 0.16.0

### Breaking Changes

- [`model_performance()`](https://easystats.github.io/performance/reference/model_performance.md)
  for psych FA objects now correctly names the metric as `RMSR` (Root
  Mean Square Residual) instead of `RMSA`. The `RMSR_corrected` column
  (previously `RMSA_corrected`) is also renamed accordingly.

- The first argument in
  [`check_model()`](https://easystats.github.io/performance/reference/check_model.md),
  [`check_predictions()`](https://easystats.github.io/performance/reference/check_predictions.md)
  and
  [`check_convergence()`](https://easystats.github.io/performance/reference/check_convergence.md)
  was renamed to `model`.

### Changes

- [`check_model()`](https://easystats.github.io/performance/reference/check_model.md)
  now limits the number of data points for models with many
  observations, to reduce the time for rendering the plot via the
  `maximum_dots` argument.

- [`check_model()`](https://easystats.github.io/performance/reference/check_model.md)
  can now show or hide confidence intervals using the `show_ci`
  argument. For models with only categorical predictors, confidence
  intervals are not shown by default.

### Bug fixes

- Fixed issue in
  [`check_dag()`](https://easystats.github.io/performance/reference/check_dag.md)
  with multiple colliders.

- Fixed CRAN check issues.

## performance 0.15.3

CRAN release: 2025-12-01

### Changes

- [`check_autocorrelation()`](https://easystats.github.io/performance/reference/check_autocorrelation.md)
  gets methods for `DHARMa` objects and objects from
  [`simulate_residuals()`](https://easystats.github.io/performance/reference/simulate_residuals.md).

- Improved documentation for printing-methods.

## performance 0.15.2

CRAN release: 2025-10-06

### Bug fixes

- Fixed failing CRAN checks, related to the latest *rstanarm* update.

## performance 0.15.1

CRAN release: 2025-08-30

### Changes

- [`display()`](https://easystats.github.io/insight/reference/display.html)
  now supports the `tinytable` format, when `format = "tt"`.

- Better handling of non-converged lavaan-models in
  [`model_performance()`](https://easystats.github.io/performance/reference/model_performance.md).

## performance 0.15.0

CRAN release: 2025-07-10

### New functions

- [`item_omega()`](https://easystats.github.io/performance/reference/item_omega.md),
  to calculate the McDonald’s Omega reliability coefficient.

- [`item_totalcor()`](https://easystats.github.io/performance/reference/item_discrimination.md)
  calculates the total correlation of an item with the sum of all other
  items in a scale. If `corrected = TRUE`, the total correlation is
  corrected for the number of items in the scale (which is equivalent to
  [`item_discrimination()`](https://easystats.github.io/performance/reference/item_discrimination.md)).

- Column names of
  [`item_reliability()`](https://easystats.github.io/performance/reference/item_reliability.md)
  were changed to be in line with the *easystats* naming convention and
  to be consistent with the output of other related functions.

### Changes

- [`check_itemscale()`](https://easystats.github.io/performance/reference/check_itemscale.md)
  now work with factor analysis results, from
  [`parameters::factor_analysis()`](https://easystats.github.io/parameters/reference/principal_components.html).

- [`item_reliability()`](https://easystats.github.io/performance/reference/item_reliability.md)
  now includes the item-total correlation, and information about
  Cronbach’s alpha and mean inter-item correlation in the printed
  output.

- [`cronbachs_alpha()`](https://easystats.github.io/performance/reference/cronbachs_alpha.md)
  now work with factor analysis results, from
  [`parameters::factor_analysis()`](https://easystats.github.io/parameters/reference/principal_components.html).

- Formatting of p-values in
  [`test_likelihoodratio()`](https://easystats.github.io/performance/reference/test_performance.md)
  is now consistent with formatted p-values from other functions.

- Added following methods for
  [`psych::fa()`](https://rdrr.io/pkg/psych/man/fa.html),
  [`psych::principal()`](https://rdrr.io/pkg/psych/man/principal.html),
  [`item_omega()`](https://easystats.github.io/performance/reference/item_omega.md),
  [`psych::omega()`](https://rdrr.io/pkg/psych/man/omega.html), and
  [`parameters::factor_analysis()`](https://easystats.github.io/parameters/reference/principal_components.html):
  [`check_normality()`](https://easystats.github.io/performance/reference/check_normality.md),
  [`check_residuals()`](https://easystats.github.io/performance/reference/check_residuals.md),
  [`check_outliers()`](https://easystats.github.io/performance/reference/check_outliers.md),
  and
  [`model_performance()`](https://easystats.github.io/performance/reference/model_performance.md).

- [`item_alpha()`](https://easystats.github.io/performance/reference/cronbachs_alpha.md)
  was added as an alias for
  [`cronbachs_alpha()`](https://easystats.github.io/performance/reference/cronbachs_alpha.md).

- Further functions get a
  [`display()`](https://easystats.github.io/insight/reference/display.html),
  [`print_md()`](https://easystats.github.io/insight/reference/display.html)
  and
  [`print_html()`](https://easystats.github.io/insight/reference/display.html)
  method.

### Bug fixes

- Fixed issue in
  [`check_predictions()`](https://easystats.github.io/performance/reference/check_predictions.md)
  for binomial models with a response defined as proportion or matrix of
  successes and trials.

- [`print_md()`](https://easystats.github.io/insight/reference/display.html)
  for objects returned by
  [`check_itemscale()`](https://easystats.github.io/performance/reference/check_itemscale.md)
  now include the footer with information about Cronbach’s alpha and
  mean inter-item correlation.

## performance 0.14.0

CRAN release: 2025-05-22

### Breaking Changes

- The `"Increased SE"` column in the output of
  [`check_collinearity()`](https://easystats.github.io/performance/reference/check_collinearity.md)
  was renamed into `"adj. VIF"` (=adjusted VIF). Furthermore, the
  computation of the adjusted VIF now correctly accounts for the numbers
  of levels (i.e. degrees of freedom) for factors.

### New functions

- New function
  [`check_group_variation()`](https://easystats.github.io/performance/reference/check_group_variation.md)
  to check within-/between-group variability (this function will replace
  [`check_heterogeneity_bias()`](https://easystats.github.io/performance/reference/check_heterogeneity_bias.md)
  in future releases.)

- New functions
  [`performance_reliability()`](https://easystats.github.io/performance/reference/performance_reliability.md)
  and
  [`performance_dvour()`](https://easystats.github.io/performance/reference/performance_reliability.md).
  These functions provide information about the reliability of
  group-level estimates (i.e., random effects) in mixed models.

### Changes

- Singularity checks with
  [`check_singularity()`](https://easystats.github.io/performance/reference/check_singularity.md)
  are now more efficient and also include the random effects for the
  dispersion component (from package *glmmTMB*). Furthermore, a `check`
  argument allows to check for general singularity (for the full model),
  or can return singularity checks for each random effects term
  separately.

### Bug fixes

- Fixed issue with wrong computation of pseudo-R2 for some models where
  the base-model (null model) was updated using the original data, which
  could include missing values. Now the model frame is used, ensuring
  the correct number of observations in the returned base-model, thus
  calculating the correct log-likelihood and returning the correct
  pseudo-R2.

- Fixed examples in
  [`check_outliers()`](https://easystats.github.io/performance/reference/check_outliers.md).

## performance 0.13.0

CRAN release: 2025-01-15

### Breaking changes

- [`check_outliers()`](https://easystats.github.io/performance/reference/check_outliers.md)
  with `method = "optics"` now returns a further refined cluster
  selection, by passing the `optics_xi` argument to
  [`dbscan::extractXi()`](https://rdrr.io/pkg/dbscan/man/optics.html).

- Deprecated arguments and alias-function-names have been removed.

- Argument names in
  [`check_model()`](https://easystats.github.io/performance/reference/check_model.md)
  that refer to plot-aesthetics (like `dot_size`) are now harmonized
  across *easystats* packages, meaning that these have been renamed.
  They now follow the pattern `aesthetic_type`, e.g. `size_dot` (instead
  of `dot_size`).

### Changes

- Increased accuracy for
  [`check_convergence()`](https://easystats.github.io/performance/reference/check_convergence.md)
  for *glmmTMB* models.

- [`r2()`](https://easystats.github.io/performance/reference/r2.md) and
  [`r2_mcfadden()`](https://easystats.github.io/performance/reference/r2_mcfadden.md)
  now support beta-binomial (non-mixed) models from package *glmmTMB*.

- An [`as.numeric()`](https://rdrr.io/r/base/numeric.html) resp.
  [`as.double()`](https://rdrr.io/r/base/double.html) method for objects
  of class `performance_roc` was added.

- Improved documentation for
  [`performance_roc()`](https://easystats.github.io/performance/reference/performance_roc.md).

### Bug fixes

- [`check_outliers()`](https://easystats.github.io/performance/reference/check_outliers.md)
  did not warn that no numeric variables were found when only the
  response variable was numeric, but all relevant predictors were not.

- [`check_collinearity()`](https://easystats.github.io/performance/reference/check_collinearity.md)
  did not work for glmmTMB models when zero-inflation component was set
  to `~0`.

## performance 0.12.4

CRAN release: 2024-10-18

### Changes

- [`check_dag()`](https://easystats.github.io/performance/reference/check_dag.md)
  now also checks for colliders, and suggests removing it in the printed
  output.

- Minor revisions to the printed output of
  [`check_dag()`](https://easystats.github.io/performance/reference/check_dag.md).

### Bug fixes

- Fixed failing tests that broke due to changes in latest *glmmTMB*
  update.

## performance 0.12.3

CRAN release: 2024-09-02

### New functions

- [`check_dag()`](https://easystats.github.io/performance/reference/check_dag.md),
  to check DAGs for correct adjustment sets.

### Changes

- [`check_heterogeneity_bias()`](https://easystats.github.io/performance/reference/check_heterogeneity_bias.md)
  gets a `nested` argument. Furthermore, `by` can specify more than one
  variable, meaning that nested or cross-classified model designs can
  also be tested for heterogeneity bias.

## performance 0.12.2

CRAN release: 2024-07-18

Patch release, to ensure that *performance* runs with older version of
*datawizard* on Mac OSX with R (old-release).

## performance 0.12.1

CRAN release: 2024-07-15

### General

- [`icc()`](https://easystats.github.io/performance/reference/icc.md)
  and
  [`r2_nakagawa()`](https://easystats.github.io/performance/reference/r2_nakagawa.md)
  get a `null_model` argument. This can be useful when computing R2 or
  ICC for mixed models, where the internal computation of the null model
  fails, or when you already have fit the null model and want to save
  time.

- [`icc()`](https://easystats.github.io/performance/reference/icc.md)
  and
  [`r2_nakagawa()`](https://easystats.github.io/performance/reference/r2_nakagawa.md)
  get a `approximation` argument indicating the approximation method for
  the distribution-specific (residual) variance. See Nakagawa et
  al. 2017 for details.

- [`icc()`](https://easystats.github.io/performance/reference/icc.md)
  and
  [`r2_nakagawa()`](https://easystats.github.io/performance/reference/r2_nakagawa.md)
  get a `model_component` argument indicating the component for
  zero-inflation or hurdle models.

- [`performance_rmse()`](https://easystats.github.io/performance/reference/performance_rmse.md)
  (resp.
  [`rmse()`](https://easystats.github.io/performance/reference/performance_rmse.md))
  can now compute analytical and bootstrapped confidence intervals. The
  function gains following new arguments: `ci`, `ci_method` and
  `iterations`.

- New function
  [`r2_ferrari()`](https://easystats.github.io/performance/reference/r2_ferrari.md)
  to compute Ferrari & Cribari-Neto’s R2 for generalized linear models,
  in particular beta-regression.

- Improved documentation of some functions.

### Bug fixes

- Fixed issue in
  [`check_model()`](https://easystats.github.io/performance/reference/check_model.md)
  when model contained a transformed response variable that was named
  like a valid R function name (e.g., `lm(log(lapply) ~ x)`, when data
  contained a variable named `lapply`).

- Fixed issue in
  [`check_predictions()`](https://easystats.github.io/performance/reference/check_predictions.md)
  for linear models when response was transformed as ratio
  (e.g. `lm(succes/trials ~ x)`).

- Fixed issue in
  [`r2_bayes()`](https://easystats.github.io/performance/reference/r2_bayes.md)
  for mixed models from *rstanarm*.

## performance 0.12.0

CRAN release: 2024-06-08

### Breaking

- Aliases `posterior_predictive_check()` and
  `check_posterior_predictions()` for
  [`check_predictions()`](https://easystats.github.io/performance/reference/check_predictions.md)
  are deprecated.

- Arguments named `group` or `group_by` will be deprecated in a future
  release. Please use `by` instead. This affects
  [`check_heterogeneity_bias()`](https://easystats.github.io/performance/reference/check_heterogeneity_bias.md)
  in *performance*.

### General

- Improved documentation and new vignettes added.

- [`check_model()`](https://easystats.github.io/performance/reference/check_model.md)
  gets a `base_size` argument, to set the base font size for plots.

- [`check_predictions()`](https://easystats.github.io/performance/reference/check_predictions.md)
  for `stanreg` and `brmsfit` models now returns plots in the usual
  style as for other models and no longer returns plots from
  [`bayesplot::pp_check()`](https://mc-stan.org/bayesplot/reference/pp_check.html).

- Updated the trained model that is used to prediction distributions in
  [`check_distribution()`](https://easystats.github.io/performance/reference/check_distribution.md).

### Bug fixes

- [`check_model()`](https://easystats.github.io/performance/reference/check_model.md)
  now falls back on normal Q-Q plots when a model is not supported by
  the DHARMa package and simulated residuals cannot be calculated.

## performance 0.11.0

CRAN release: 2024-03-22

### New supported models

- Rudimentary support for models of class `serp` from package *serp*.

### New functions

- [`simulate_residuals()`](https://easystats.github.io/performance/reference/simulate_residuals.md)
  and
  [`check_residuals()`](https://easystats.github.io/performance/reference/check_residuals.md),
  to simulate and check residuals from generalized linear (mixed)
  models. Simulating residuals is based on the DHARMa package, and
  objects returned by
  [`simulate_residuals()`](https://easystats.github.io/performance/reference/simulate_residuals.md)
  inherit from the `DHARMa` class, and thus can be used with any
  functions from the *DHARMa* package. However, there are also
  implementations in the *performance* package, such as
  [`check_overdispersion()`](https://easystats.github.io/performance/reference/check_overdispersion.md),
  [`check_zeroinflation()`](https://easystats.github.io/performance/reference/check_zeroinflation.md),
  [`check_outliers()`](https://easystats.github.io/performance/reference/check_outliers.md)
  or
  [`check_model()`](https://easystats.github.io/performance/reference/check_model.md).

- Plots for
  [`check_model()`](https://easystats.github.io/performance/reference/check_model.md)
  have been improved. The Q-Q plots are now based on simulated residuals
  from the DHARMa package for non-Gaussian models, thus providing more
  accurate and informative plots. The half-normal QQ plot for
  generalized linear models can still be obtained by setting the new
  argument `residual_type = "normal"`.

- Following functions now support simulated residuals (from
  [`simulate_residuals()`](https://easystats.github.io/performance/reference/simulate_residuals.md))
  resp. objects returned from
  [`DHARMa::simulateResiduals()`](https://rdrr.io/pkg/DHARMa/man/simulateResiduals.html):

  - [`check_overdispersion()`](https://easystats.github.io/performance/reference/check_overdispersion.md)
  - [`check_zeroinflation()`](https://easystats.github.io/performance/reference/check_zeroinflation.md)
  - [`check_outliers()`](https://easystats.github.io/performance/reference/check_outliers.md)
  - [`check_model()`](https://easystats.github.io/performance/reference/check_model.md)

### General

- Improved error messages for
  [`check_model()`](https://easystats.github.io/performance/reference/check_model.md)
  when QQ-plots cannot be created.

- [`check_distribution()`](https://easystats.github.io/performance/reference/check_distribution.md)
  is more stable for possibly sparse data.

### Bug fixes

- Fixed issue in
  [`check_normality()`](https://easystats.github.io/performance/reference/check_normality.md)
  for t-tests.

- Fixed issue in
  [`check_itemscale()`](https://easystats.github.io/performance/reference/check_itemscale.md)
  for data frame inputs, when `factor_index` was not a named vector.

## performance 0.10.9

CRAN release: 2024-02-17

### Changes

- [`r2()`](https://easystats.github.io/performance/reference/r2.md) for
  models of class `glmmTMB` without random effects now returns the
  correct r-squared value for non-mixed models.

- [`check_itemscale()`](https://easystats.github.io/performance/reference/check_itemscale.md)
  now also accepts data frames as input. In this case, `factor_index`
  must be specified, which must be a numeric vector of same length as
  number of columns in `x`, where each element is the index of the
  factor to which the respective column in `x`.

- [`check_itemscale()`](https://easystats.github.io/performance/reference/check_itemscale.md)
  gets a
  [`print_html()`](https://easystats.github.io/insight/reference/display.html)
  method.

- Clarification in the documentation of the `estimator` argument for
  [`performance_aic()`](https://easystats.github.io/performance/reference/performance_aicc.md).

- Improved plots for overdispersion-checks for negative-binomial models
  from package *glmmTMB* (affects
  [`check_overdispersion()`](https://easystats.github.io/performance/reference/check_overdispersion.md)
  and
  [`check_model()`](https://easystats.github.io/performance/reference/check_model.md)).

- Improved detection rates for singularity in
  [`check_singularity()`](https://easystats.github.io/performance/reference/check_singularity.md)
  for models from package *glmmTMB*.

- For model of class `glmmTMB`, deviance residuals are now used in the
  [`check_model()`](https://easystats.github.io/performance/reference/check_model.md)
  plot.

- Improved (better to understand) error messages for
  [`check_model()`](https://easystats.github.io/performance/reference/check_model.md),
  [`check_collinearity()`](https://easystats.github.io/performance/reference/check_collinearity.md)
  and
  [`check_outliers()`](https://easystats.github.io/performance/reference/check_outliers.md)
  for models with non-numeric response variables.

- [`r2_kullback()`](https://easystats.github.io/performance/reference/r2_kullback.md)
  now gives an informative error for non-supported models.

### Bug fixes

- Fixed issue in
  [`binned_residuals()`](https://easystats.github.io/performance/reference/binned_residuals.md)
  for models with binary outcome, where in rare occasions empty bins
  could occur.

- [`performance_score()`](https://easystats.github.io/performance/reference/performance_score.md)
  should no longer fail for models where scoring rules can’t be
  calculated. Instead, an informative message is returned.

- [`check_outliers()`](https://easystats.github.io/performance/reference/check_outliers.md)
  now properly accept the `percentage_central` argument when using the
  `"mcd"` method.

- Fixed edge cases in
  [`check_collinearity()`](https://easystats.github.io/performance/reference/check_collinearity.md)
  and
  [`check_outliers()`](https://easystats.github.io/performance/reference/check_outliers.md)
  for models with response variables of classes `Date`, `POSIXct`,
  `POSIXlt` or `difftime`.

- Fixed issue with
  [`check_model()`](https://easystats.github.io/performance/reference/check_model.md)
  for models of package *quantreg*.

## performance 0.10.8

CRAN release: 2023-10-30

### Changes

- Changed behaviour of
  [`check_predictions()`](https://easystats.github.io/performance/reference/check_predictions.md)
  for models from binomial family, to get comparable plots for different
  ways of outcome specification. Now, if the outcome is a proportion, or
  defined as matrix of trials and successes, the produced plots are the
  same (because the models should be the same, too).

### Bug fixes

- Fixed CRAN check errors.

- Fixed issue with
  [`binned_residuals()`](https://easystats.github.io/performance/reference/binned_residuals.md)
  for models with binomial family, where the outcome was a proportion.

## performance 0.10.7

CRAN release: 2023-10-27

### Breaking changes

- [`binned_residuals()`](https://easystats.github.io/performance/reference/binned_residuals.md)
  gains a few new arguments to control the residuals used for the test,
  as well as different options to calculate confidence intervals
  (namely, `ci_type`, `residuals`, `ci` and `iterations`). The default
  values to compute binned residuals have changed. Default residuals are
  now “deviance” residuals (and no longer “response” residuals). Default
  confidence intervals are now “exact” intervals (and no longer based on
  Gaussian approximation). Use `ci_type = "gaussian"` and
  `residuals = "response"` to get the old defaults.

### Changes to functions

- [`binned_residuals()`](https://easystats.github.io/performance/reference/binned_residuals.md) -
  like
  [`check_model()`](https://easystats.github.io/performance/reference/check_model.md) -
  gains a `show_dots` argument to show or hide data points that lie
  inside error bounds. This is particular useful for models with many
  observations, where generating the plot would be very slow.

## performance 0.10.6

### General

- Support for `nestedLogit` models.

### Changes to functions

- [`check_outliers()`](https://easystats.github.io/performance/reference/check_outliers.md)
  for method `"ics"` now detects number of available cores for parallel
  computing via the `"mc.cores"` option. This is more robust than the
  previous method, which used
  [`parallel::detectCores()`](https://rdrr.io/r/parallel/detectCores.html).
  Now you should set the number of cores via `options(mc.cores = 4)`.

### Bug fixes

- Fixed issues is
  [`check_model()`](https://easystats.github.io/performance/reference/check_model.md)
  for models that used data sets with variables of class
  `"haven_labelled"`.

## performance 0.10.5

CRAN release: 2023-09-12

### Changes to functions

- More informative message for `test_*()` functions that “nesting” only
  refers to fixed effects parameters and currently ignores random
  effects when detecting nested models.

- [`check_outliers()`](https://easystats.github.io/performance/reference/check_outliers.md)
  for `"ICS"` method is now more stable and less likely to fail.

- [`check_convergence()`](https://easystats.github.io/performance/reference/check_convergence.md)
  now works for *parsnip* `_glm` models.

### Bug fixes

- [`check_collinearity()`](https://easystats.github.io/performance/reference/check_collinearity.md)
  did not work for hurdle- or zero-inflated models of package *pscl*
  when model had no explicitly defined formula for the zero-inflation
  model.

## performance 0.10.4

CRAN release: 2023-06-02

### Changes to functions

- [`icc()`](https://easystats.github.io/performance/reference/icc.md)
  and
  [`r2_nakagawa()`](https://easystats.github.io/performance/reference/r2_nakagawa.md)
  gain a `ci_method` argument, to either calculate confidence intervals
  using [`boot::boot()`](https://rdrr.io/pkg/boot/man/boot.html)
  (instead of `lmer::bootMer()`) when `ci_method = "boot"` or analytical
  confidence intervals (`ci_method = "analytical"`). Use
  `ci_method = "boot"` when the default method fails to compute
  confidence intervals and use `ci_method = "analytical"` if
  bootstrapped intervals cannot be calculated at all. Note that the
  default computation method is preferred.

- [`check_predictions()`](https://easystats.github.io/performance/reference/check_predictions.md)
  accepts a `bandwidth` argument (smoothing bandwidth), which is passed
  down to the [`plot()`](https://rdrr.io/r/graphics/plot.default.html)
  methods density-estimation.

- [`check_predictions()`](https://easystats.github.io/performance/reference/check_predictions.md)
  gains a `type` argument, which is passed down to the
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) method to
  change plot-type (density or discrete dots/intervals). By default,
  `type` is set to `"default"` for models without discrete outcomes, and
  else `type = "discrete_interval"`.

- [`performance_accuracy()`](https://easystats.github.io/performance/reference/performance_accuracy.md)
  now includes confidence intervals, and reports those by default (the
  standard error is no longer reported, but still included).

### Bug fixes

- Fixed issue in
  [`check_collinearity()`](https://easystats.github.io/performance/reference/check_collinearity.md)
  for *fixest* models that used `i()` to create interactions in
  formulas.

## performance 0.10.3

CRAN release: 2023-04-07

### New functions

- [`item_discrimination()`](https://easystats.github.io/performance/reference/item_discrimination.md),
  to calculate the discrimination of a scale’s items.

### Support for new models

- [`model_performance()`](https://easystats.github.io/performance/reference/model_performance.md),
  [`check_overdispersion()`](https://easystats.github.io/performance/reference/check_overdispersion.md),
  [`check_outliers()`](https://easystats.github.io/performance/reference/check_outliers.md)
  and [`r2()`](https://easystats.github.io/performance/reference/r2.md)
  now work with objects of class `fixest_multi`
  ([@etiennebacher](https://github.com/etiennebacher),
  [\#554](https://github.com/easystats/performance/issues/554)).

- [`model_performance()`](https://easystats.github.io/performance/reference/model_performance.md)
  can now return the “Weak instruments” statistic and p-value for models
  of class `ivreg` with `metrics = "weak_instruments"`
  ([@etiennebacher](https://github.com/etiennebacher),
  [\#560](https://github.com/easystats/performance/issues/560)).

- Support for `mclogit` models.

### Changes to functions

- `test_*()` functions now automatically fit a null-model when only one
  model objects was provided for testing multiple models.

- Warnings in
  [`model_performance()`](https://easystats.github.io/performance/reference/model_performance.md)
  for unsupported objects of class `BFBayesFactor` can now be suppressed
  with `verbose = FALSE`.

- [`check_predictions()`](https://easystats.github.io/performance/reference/check_predictions.md)
  no longer fails with issues when `re_formula = NULL` for mixed models,
  but instead gives a warning and tries to compute posterior predictive
  checks with `re_formuka = NA`.

- [`check_outliers()`](https://easystats.github.io/performance/reference/check_outliers.md)
  now also works for meta-analysis models from packages *metafor* and
  *meta*.

- [`plot()`](https://rdrr.io/r/graphics/plot.default.html) for
  [`performance::check_model()`](https://easystats.github.io/performance/reference/check_model.md)
  no longer produces a normal QQ plot for GLMs. Instead, it now shows a
  half-normal QQ plot of the absolute value of the standardized deviance
  residuals.

### Bug fixes

- Fixed issue in [`print()`](https://rdrr.io/r/base/print.html) method
  for
  [`check_collinearity()`](https://easystats.github.io/performance/reference/check_collinearity.md),
  which could mix up the correct order of parameters.

## performance 0.10.2

CRAN release: 2023-01-12

### General

- Revised usage of
  [`insight::get_data()`](https://easystats.github.io/insight/reference/get_data.html)
  to meet forthcoming changes in the *insight* package.

### Changes to functions

- [`check_collinearity()`](https://easystats.github.io/performance/reference/check_collinearity.md)
  now accepts `NULL` for the `ci` argument.

### Bug fixes

- Fixed issue in
  [`item_difficulty()`](https://easystats.github.io/performance/reference/item_difficulty.md)
  with detecting the maximum values of an item set. Furthermore,
  [`item_difficulty()`](https://easystats.github.io/performance/reference/item_difficulty.md)
  gets a `maximum_value` argument in case no item contains the maximum
  value due to missings.

## performance 0.10.1

CRAN release: 2022-11-25

### General

- Minor improvements to the documentation.

### Changes to functions

- [`icc()`](https://easystats.github.io/performance/reference/icc.md)
  and
  [`r2_nakagawa()`](https://easystats.github.io/performance/reference/r2_nakagawa.md)
  get `ci` and `iterations` arguments, to compute confidence intervals
  for the ICC resp. R2, based on bootstrapped sampling.

- [`r2()`](https://easystats.github.io/performance/reference/r2.md) gets
  `ci`, to compute (analytical) confidence intervals for the R2.

- The model underlying
  [`check_distribution()`](https://easystats.github.io/performance/reference/check_distribution.md)
  was now also trained to detect cauchy, half-cauchy and inverse-gamma
  distributions.

- [`model_performance()`](https://easystats.github.io/performance/reference/model_performance.md)
  now allows to include the ICC for Bayesian models.

### Bug fixes

- `verbose` didn’t work for
  [`r2_bayes()`](https://easystats.github.io/performance/reference/r2_bayes.md)
  with `BFBayesFactor` objects.

- Fixed issues in
  [`check_model()`](https://easystats.github.io/performance/reference/check_model.md)
  for models with convergence issues that lead to `NA` values in
  residuals.

- Fixed bug in `check_outliers` whereby passing multiple elements to the
  threshold list generated an error
  ([\#496](https://github.com/easystats/performance/issues/496)).

- [`test_wald()`](https://easystats.github.io/performance/reference/test_performance.md)
  now warns the user about inappropriate F test and calls
  [`test_likelihoodratio()`](https://easystats.github.io/performance/reference/test_performance.md)
  for binomial models.

- Fixed edge case for usage of `parellel::detectCores()` in
  [`check_outliers()`](https://easystats.github.io/performance/reference/check_outliers.md).

## performance 0.10.0

CRAN release: 2022-10-03

### Breaking Change

- The minimum needed R version has been bumped to `3.6`.

- The alias `performance_lrt()` was removed. Use
  [`test_lrt()`](https://easystats.github.io/performance/reference/test_performance.md)
  resp.
  [`test_likelihoodratio()`](https://easystats.github.io/performance/reference/test_performance.md).

### New functions

- Following functions were moved from package *parameters* to
  *performance*:
  [`check_sphericity_bartlett()`](https://easystats.github.io/performance/reference/check_factorstructure.md),
  [`check_kmo()`](https://easystats.github.io/performance/reference/check_factorstructure.md),
  [`check_factorstructure()`](https://easystats.github.io/performance/reference/check_factorstructure.md)
  and
  [`check_clusterstructure()`](https://easystats.github.io/performance/reference/check_clusterstructure.md).

### Changes to functions

- [`check_normality()`](https://easystats.github.io/performance/reference/check_normality.md),
  [`check_homogeneity()`](https://easystats.github.io/performance/reference/check_homogeneity.md)
  and
  [`check_symmetry()`](https://easystats.github.io/performance/reference/check_symmetry.md)
  now works for `htest` objects.

- Print method for
  [`check_outliers()`](https://easystats.github.io/performance/reference/check_outliers.md)
  changed significantly: now states the methods, thresholds, and
  variables used, reports outliers per variable (for univariate methods)
  as well as any observation flagged for several variables/methods.
  Includes a new optional ID argument to add along the row number in the
  output ([@rempsyc](https://github.com/rempsyc)
  [\#443](https://github.com/easystats/performance/issues/443)).

- [`check_outliers()`](https://easystats.github.io/performance/reference/check_outliers.md)
  now uses more conventional outlier thresholds. The `IQR` and
  confidence interval methods now gain improved distance scores that are
  continuous instead of discrete.

### Bug Fixes

- Fixed wrong *z*-score values when using a vector instead of a data
  frame in
  [`check_outliers()`](https://easystats.github.io/performance/reference/check_outliers.md)
  ([\#476](https://github.com/easystats/performance/issues/476)).

- Fixed
  [`cronbachs_alpha()`](https://easystats.github.io/performance/reference/cronbachs_alpha.md)
  for objects from `parameters::principal_component()`.

## performance 0.9.2

CRAN release: 2022-08-10

### General

- [`print()`](https://rdrr.io/r/base/print.html) methods for
  [`model_performance()`](https://easystats.github.io/performance/reference/model_performance.md)
  and
  [`compare_performance()`](https://easystats.github.io/performance/reference/compare_performance.md)
  get a `layout` argument, which can be `"horizontal"` (default) or
  `"vertical"`, to switch the layout of the printed table.

- Improved speed performance for
  [`check_model()`](https://easystats.github.io/performance/reference/check_model.md)
  and some other `performance_*()` functions.

- Improved support for models of class `geeglm`.

### Changes to functions

- [`check_model()`](https://easystats.github.io/performance/reference/check_model.md)
  gains a `show_dots` argument, to show or hide data points. This is
  particular useful for models with many observations, where generating
  the plot would be very slow.

### Bug Fixes

- Fixes wrong column names in
  [`model_performance()`](https://easystats.github.io/performance/reference/model_performance.md)
  output for `kmeans` objects
  ([\#453](https://github.com/easystats/performance/issues/453))

## performance 0.9.1

CRAN release: 2022-06-20

### Breaking

- The formerly “conditional” ICC in
  [`icc()`](https://easystats.github.io/performance/reference/icc.md) is
  now named “unadjusted” ICC.

### New functions

- [`performance_cv()`](https://easystats.github.io/performance/reference/performance_cv.md)
  for cross-validated model performance.

### Support for new models

- Added support for models from package *estimator*.

### Changes to functions

- [`check_overdispersion()`](https://easystats.github.io/performance/reference/check_overdispersion.md)
  gets a [`plot()`](https://rdrr.io/r/graphics/plot.default.html)
  method.

- [`check_outliers()`](https://easystats.github.io/performance/reference/check_outliers.md)
  now also works for models of classes `gls` and `lme`. As a
  consequence,
  [`check_model()`](https://easystats.github.io/performance/reference/check_model.md)
  will no longer fail for these models.

- [`check_collinearity()`](https://easystats.github.io/performance/reference/check_collinearity.md)
  now includes the confidence intervals for the VIFs and tolerance
  values.

- [`model_performance()`](https://easystats.github.io/performance/reference/model_performance.md)
  now also includes within-subject R2 measures, where applicable.

- Improved handling of random effects in
  [`check_normality()`](https://easystats.github.io/performance/reference/check_normality.md)
  (i.e. when argument `effects = "random"`).

### Bug fixes

- [`check_predictions()`](https://easystats.github.io/performance/reference/check_predictions.md)
  did not work for GLMs with matrix-response.

- [`check_predictions()`](https://easystats.github.io/performance/reference/check_predictions.md)
  did not work for logistic regression models (i.e. models with binary
  response) from package *glmmTMB*

- [`item_split_half()`](https://easystats.github.io/performance/reference/item_split_half.md)
  did not work when the input data frame or matrix only contained two
  columns.

- Fixed wrong computation of `BIC` in
  [`model_performance()`](https://easystats.github.io/performance/reference/model_performance.md)
  when models had transformed response values.

- Fixed issues in
  [`check_model()`](https://easystats.github.io/performance/reference/check_model.md)
  for GLMs with matrix-response.

## performance 0.9.0

CRAN release: 2022-03-30

### New functions

- [`check_concurvity()`](https://easystats.github.io/performance/reference/check_collinearity.md),
  which returns GAM concurvity measures (comparable to collinearity
  checks).

### Changes to functions

#### Check functions

- [`check_predictions()`](https://easystats.github.io/performance/reference/check_predictions.md),
  [`check_collinearity()`](https://easystats.github.io/performance/reference/check_collinearity.md)
  and
  [`check_outliers()`](https://easystats.github.io/performance/reference/check_outliers.md)
  now support (mixed) regression models from `BayesFactor`.

- [`check_zeroinflation()`](https://easystats.github.io/performance/reference/check_zeroinflation.md)
  now also works for
  [`lme4::glmer.nb()`](https://rdrr.io/pkg/lme4/man/glmer.nb.html)
  models.

- [`check_collinearity()`](https://easystats.github.io/performance/reference/check_collinearity.md)
  better supports GAM models.

#### Test functions

- [`test_performance()`](https://easystats.github.io/performance/reference/test_performance.md)
  now calls
  [`test_lrt()`](https://easystats.github.io/performance/reference/test_performance.md)
  or
  [`test_wald()`](https://easystats.github.io/performance/reference/test_performance.md)
  instead of
  [`test_vuong()`](https://easystats.github.io/performance/reference/test_performance.md)
  when package *CompQuadForm* is missing.

- [`test_performance()`](https://easystats.github.io/performance/reference/test_performance.md)
  and
  [`test_lrt()`](https://easystats.github.io/performance/reference/test_performance.md)
  now compute the corrected log-likelihood when models with transformed
  response variables (such as log- or sqrt-transformations) are passed
  to the functions.

#### Model performance functions

- [`performance_aic()`](https://easystats.github.io/performance/reference/performance_aicc.md)
  now corrects the AIC value for models with transformed response
  variables. This also means that comparing models using
  [`compare_performance()`](https://easystats.github.io/performance/reference/compare_performance.md)
  allows comparisons of AIC values for models with and without
  transformed response variables.

- Also,
  [`model_performance()`](https://easystats.github.io/performance/reference/model_performance.md)
  now corrects both AIC and BIC values for models with transformed
  response variables.

#### Plotting and printing

- The [`print()`](https://rdrr.io/r/base/print.html) method for
  [`binned_residuals()`](https://easystats.github.io/performance/reference/binned_residuals.md)
  now prints a short summary of the results (and no longer generates a
  plot). A [`plot()`](https://rdrr.io/r/graphics/plot.default.html)
  method was added to generate plots.

- The [`plot()`](https://rdrr.io/r/graphics/plot.default.html) output
  for
  [`check_model()`](https://easystats.github.io/performance/reference/check_model.md)
  was revised:

  - For binomial models, the constant variance plot was omitted, and a
    binned residuals plot included.

  - The density-plot that showed normality of residuals was replaced by
    the posterior predictive check plot.

### Bug fixes

- [`model_performance()`](https://easystats.github.io/performance/reference/model_performance.md)
  for models from *lme4* did not report AICc when requested.

- [`r2_nakagawa()`](https://easystats.github.io/performance/reference/r2_nakagawa.md)
  messed up order of group levels when `by_group` was `TRUE`.

## performance 0.8.0

CRAN release: 2021-10-01

### Breaking Changes

- The `ci`-level in
  [`r2()`](https://easystats.github.io/performance/reference/r2.md) for
  Bayesian models now defaults to `0.95`, to be in line with the latest
  changes in the *bayestestR* package.

- S3-method dispatch for
  [`pp_check()`](https://mc-stan.org/bayesplot/reference/pp_check.html)
  was revised, to avoid problems with the *bayesplot* package, where the
  generic is located.

### General

- Minor revisions to wording for messages from some of the
  check-functions.

- `posterior_predictive_check()` and
  [`check_predictions()`](https://easystats.github.io/performance/reference/check_predictions.md)
  were added as aliases for
  [`pp_check()`](https://mc-stan.org/bayesplot/reference/pp_check.html).

### New functions

- [`check_multimodal()`](https://easystats.github.io/performance/reference/check_multimodal.md)
  and
  [`check_heterogeneity_bias()`](https://easystats.github.io/performance/reference/check_heterogeneity_bias.md).
  These functions will be removed from the *parameters* packages in the
  future.

### Changes to functions

- [`r2()`](https://easystats.github.io/performance/reference/r2.md) for
  linear models can now compute confidence intervals, via the `ci`
  argument.

### Bug fixes

- Fixed issues in
  [`check_model()`](https://easystats.github.io/performance/reference/check_model.md)
  for Bayesian models.

- Fixed issue in
  [`pp_check()`](https://mc-stan.org/bayesplot/reference/pp_check.html)
  for models with transformed response variables, so now predictions and
  observed response values are on the same (transformed) scale.

## performance 0.7.3

CRAN release: 2021-07-21

### Changes to functions

- [`check_outliers()`](https://easystats.github.io/performance/reference/check_outliers.md)
  has new `ci` (or `hdi`, `eti`) method to filter based on
  Confidence/Credible intervals.

- [`compare_performance()`](https://easystats.github.io/performance/reference/compare_performance.md)
  now also accepts a list of model objects.

- [`performance_roc()`](https://easystats.github.io/performance/reference/performance_roc.md)
  now also works for binomial models from other classes than *glm*.

- Several functions, like
  [`icc()`](https://easystats.github.io/performance/reference/icc.md) or
  [`r2_nakagawa()`](https://easystats.github.io/performance/reference/r2_nakagawa.md),
  now have an
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) method.

- [`check_collinearity()`](https://easystats.github.io/performance/reference/check_collinearity.md)
  now correctly handles objects from forthcoming *afex* update.

## performance 0.7.2

CRAN release: 2021-05-17

### New functions

- [`performance_mae()`](https://easystats.github.io/performance/reference/performance_mae.md)
  to calculate the mean absolute error.

### Bug fixes

- Fixed issue with `"data length differs from size of matrix"` warnings
  in examples in forthcoming R 4.2.

- Fixed issue in
  [`check_normality()`](https://easystats.github.io/performance/reference/check_normality.md)
  for models with sample size larger than

5.000 observations.

- Fixed issue in
  [`check_model()`](https://easystats.github.io/performance/reference/check_model.md)
  for *glmmTMB* models.

- Fixed issue in
  [`check_collinearity()`](https://easystats.github.io/performance/reference/check_collinearity.md)
  for *glmmTMB* models with zero-inflation, where the zero-inflated
  model was an intercept-only model.

## performance 0.7.1

CRAN release: 2021-04-09

### New supported models

- Add support for `model_fit` (*tidymodels*).

- `model_performance` supports *kmeans* models.

### General

- Give more informative warning when
  [`r2_bayes()`](https://easystats.github.io/performance/reference/r2_bayes.md)
  for *BFBayesFactor* objects can’t be calculated.

- Several `check_*()` functions now return informative messages for
  invalid model types as input.

- [`r2()`](https://easystats.github.io/performance/reference/r2.md)
  supports `mhurdle` (*mhurdle*) models.

- Added [`print()`](https://rdrr.io/r/base/print.html) methods for more
  classes of
  [`r2()`](https://easystats.github.io/performance/reference/r2.md).

- The
  [`performance_roc()`](https://easystats.github.io/performance/reference/performance_roc.md)
  and
  [`performance_accuracy()`](https://easystats.github.io/performance/reference/performance_accuracy.md)
  functions unfortunately had spelling mistakes in the output columns:
  *Sensitivity* was called *Sensivity* and *Specificity* was called
  *Specifity*. We think these are understandable mistakes :-)

### Changes to functions

#### `check_model()`

- [`check_model()`](https://easystats.github.io/performance/reference/check_model.md)
  gains more arguments, to customize plot appearance.

- Added option to detrend QQ/PP plots in
  [`check_model()`](https://easystats.github.io/performance/reference/check_model.md).

#### `model_performance()`

- The `metrics` argument from
  [`model_performance()`](https://easystats.github.io/performance/reference/model_performance.md)
  and
  [`compare_performance()`](https://easystats.github.io/performance/reference/compare_performance.md)
  gains a `"AICc"` option, to also compute the 2nd order AIC.

- `"R2_adj"` is now an explicit option in the `metrics` argument from
  [`model_performance()`](https://easystats.github.io/performance/reference/model_performance.md)
  and
  [`compare_performance()`](https://easystats.github.io/performance/reference/compare_performance.md).

#### Other functions

- The default-method for
  [`r2()`](https://easystats.github.io/performance/reference/r2.md) now
  tries to compute an r-squared for all models that have no specific
  [`r2()`](https://easystats.github.io/performance/reference/r2.md)-method
  yet, by using following formula:
  `1-sum((y-y_hat)^2)/sum((y-y_bar)^2))`

- The column name `Parameter` in
  [`check_collinearity()`](https://easystats.github.io/performance/reference/check_collinearity.md)
  is now more appropriately named `Term`.

### Bug fixes

- [`test_likelihoodratio()`](https://easystats.github.io/performance/reference/test_performance.md)
  now correctly sorts models with identical fixed effects part, but
  different other model parts (like zero-inflation).

- Fixed incorrect computation of models from inverse-Gaussian families,
  or Gaussian families fitted with
  [`glm()`](https://rdrr.io/r/stats/glm.html).

- Fixed issue in
  [`performance_roc()`](https://easystats.github.io/performance/reference/performance_roc.md)
  for models where outcome was not 0/1 coded.

- Fixed issue in
  [`performance_accuracy()`](https://easystats.github.io/performance/reference/performance_accuracy.md)
  for logistic regression models when `method = "boot"`.

- [`cronbachs_alpha()`](https://easystats.github.io/performance/reference/cronbachs_alpha.md)
  did not work for `matrix`-objects, as stated in the docs. It now does.

## performance 0.7.0

CRAN release: 2021-02-03

### General

- Roll-back R dependency to R \>= 3.4.

### Breaking Changes

- [`compare_performance()`](https://easystats.github.io/performance/reference/compare_performance.md)
  doesn’t return the models’ Bayes Factors, now returned by
  [`test_performance()`](https://easystats.github.io/performance/reference/test_performance.md)
  and
  [`test_bf()`](https://easystats.github.io/performance/reference/test_performance.md).

### New functions to test or compare models

- [`test_vuong()`](https://easystats.github.io/performance/reference/test_performance.md),
  to compare models using Vuong’s (1989) Test.

- [`test_bf()`](https://easystats.github.io/performance/reference/test_performance.md),
  to compare models using Bayes factors.

- [`test_likelihoodratio()`](https://easystats.github.io/performance/reference/test_performance.md)
  as an alias for `performance_lrt()`.

- [`test_wald()`](https://easystats.github.io/performance/reference/test_performance.md),
  as a rough approximation for the LRT.

- [`test_performance()`](https://easystats.github.io/performance/reference/test_performance.md),
  to run the most relevant and appropriate tests based on the input.

### Changes to functions

#### `performance_lrt()`

- `performance_lrt()` get an alias
  [`test_likelihoodratio()`](https://easystats.github.io/performance/reference/test_performance.md).

- Does not return AIC/BIC now (as they are not related to LRT *per se*
  and can be easily obtained with other functions).

- Now contains a column with the difference in degrees of freedom
  between models.

- Fixed column names for consistency.

#### `model_performance()`

- Added more diagnostics to models of class `ivreg`.

#### Other functions

- Revised computation of
  [`performance_mse()`](https://easystats.github.io/performance/reference/performance_mse.md),
  to ensure that it’s always based on response residuals.

- [`performance_aic()`](https://easystats.github.io/performance/reference/performance_aicc.md)
  is now more robust.

### Bug fixes

- Fixed issue in
  [`icc()`](https://easystats.github.io/performance/reference/icc.md)
  and
  [`variance_decomposition()`](https://easystats.github.io/performance/reference/icc.md)
  for multivariate response models, where not all model parts contained
  random effects.

- Fixed issue in
  [`compare_performance()`](https://easystats.github.io/performance/reference/compare_performance.md)
  with duplicated rows.

- [`check_collinearity()`](https://easystats.github.io/performance/reference/check_collinearity.md)
  no longer breaks for models with rank deficient model matrix, but
  gives a warning instead.

- Fixed issue in
  [`check_homogeneity()`](https://easystats.github.io/performance/reference/check_homogeneity.md)
  for `method = "auto"`, which wrongly tested the response variable, not
  the residuals.

- Fixed issue in
  [`check_homogeneity()`](https://easystats.github.io/performance/reference/check_homogeneity.md)
  for edge cases where predictor had non-syntactic names.

## performance 0.6.1

CRAN release: 2020-12-09

### General

- [`check_collinearity()`](https://easystats.github.io/performance/reference/check_collinearity.md)
  gains a `verbose` argument, to toggle warnings and messages.

### Bug fixes

- Fixed examples, now using suggested packages only conditionally.

## performance 0.6.0

CRAN release: 2020-12-01

### General

- [`model_performance()`](https://easystats.github.io/performance/reference/model_performance.md)
  now supports `margins`, `gamlss`, `stanmvreg` and `semLme`.

### New functions

- [`r2_somers()`](https://easystats.github.io/performance/reference/r2_somers.md),
  to compute Somers’ Dxy rank-correlation as R2-measure for logistic
  regression models.

- [`display()`](https://easystats.github.io/insight/reference/display.html),
  to print output from package-functions into different formats.
  [`print_md()`](https://easystats.github.io/insight/reference/display.html)
  is an alias for `display(format = "markdown")`.

### Changes to functions

#### `model_performance()`

- [`model_performance()`](https://easystats.github.io/performance/reference/model_performance.md)
  is now more robust and doesn’t fail if an index could not be computed.
  Instead, it returns all indices that were possible to calculate.

- [`model_performance()`](https://easystats.github.io/performance/reference/model_performance.md)
  gains a default-method that catches all model objects not previously
  supported. If model object is also not supported by the
  default-method, a warning is given.

- [`model_performance()`](https://easystats.github.io/performance/reference/model_performance.md)
  for metafor-models now includes the degrees of freedom for Cochran’s
  Q.

#### Other functions

- [`performance_mse()`](https://easystats.github.io/performance/reference/performance_mse.md)
  and
  [`performance_rmse()`](https://easystats.github.io/performance/reference/performance_rmse.md)
  now always try to return the (R)MSE on the response scale.

- [`performance_accuracy()`](https://easystats.github.io/performance/reference/performance_accuracy.md)
  now accepts all types of linear or logistic regression models, even if
  these are not of class `lm` or `glm`.

- [`performance_roc()`](https://easystats.github.io/performance/reference/performance_roc.md)
  now accepts all types of logistic regression models, even if these are
  not of class `glm`.

- [`r2()`](https://easystats.github.io/performance/reference/r2.md) for
  mixed models and
  [`r2_nakagawa()`](https://easystats.github.io/performance/reference/r2_nakagawa.md)
  gain a `tolerance`-argument, to set the tolerance level for
  singularity checks when computing random effect variances for the
  conditional r-squared.

### Bug fixes

- Fixed issue in
  [`icc()`](https://easystats.github.io/performance/reference/icc.md)
  introduced in the last update that make `lme`-models fail.

- Fixed issue in
  [`performance_roc()`](https://easystats.github.io/performance/reference/performance_roc.md)
  for models with factors as response.

## performance 0.5.1

CRAN release: 2020-10-29

### Breaking changes

- Column names for
  [`model_performance()`](https://easystats.github.io/performance/reference/model_performance.md)
  and
  [`compare_performance()`](https://easystats.github.io/performance/reference/compare_performance.md)
  were changed to be in line with the *easystats* naming convention:
  `LOGLOSS` is now `Log_loss`, `SCORE_LOG` is `Score_log` and
  `SCORE_SPHERICAL` is now `Score_spherical`.

### New functions

- [`r2_posterior()`](https://easystats.github.io/performance/reference/r2_bayes.md)
  for Bayesian models to obtain posterior distributions of R-squared.

### Changes to functions

- [`r2_bayes()`](https://easystats.github.io/performance/reference/r2_bayes.md)
  works with Bayesian models from `BayesFactor` (
  [\#143](https://github.com/easystats/performance/issues/143) ).

- [`model_performance()`](https://easystats.github.io/performance/reference/model_performance.md)
  works with Bayesian models from `BayesFactor` (
  [\#150](https://github.com/easystats/performance/issues/150) ).

- [`model_performance()`](https://easystats.github.io/performance/reference/model_performance.md)
  now also includes the residual standard deviation.

- Improved formatting for Bayes factors in
  [`compare_performance()`](https://easystats.github.io/performance/reference/compare_performance.md).

- [`compare_performance()`](https://easystats.github.io/performance/reference/compare_performance.md)
  with `rank = TRUE` doesn’t use the `BF` values when `BIC` are present,
  to prevent “double-dipping” of the BIC values
  ([\#144](https://github.com/easystats/performance/issues/144)).

- The `method` argument in
  [`check_homogeneity()`](https://easystats.github.io/performance/reference/check_homogeneity.md)
  gains a `"levene"` option, to use Levene’s Test for homogeneity.

### Bug fixes

- Fix bug in
  [`compare_performance()`](https://easystats.github.io/performance/reference/compare_performance.md)
  when `...` arguments were function calls to regression objects,
  instead of direct function calls.

## performance 0.5.0

CRAN release: 2020-09-12

### General

- [`r2()`](https://easystats.github.io/performance/reference/r2.md) and
  [`icc()`](https://easystats.github.io/performance/reference/icc.md)
  support `semLME` models (package *smicd*).

- [`check_heteroscedasticity()`](https://easystats.github.io/performance/reference/check_heteroscedasticity.md)
  should now also work with zero-inflated mixed models from *glmmTMB*
  and *GLMMadpative*.

- [`check_outliers()`](https://easystats.github.io/performance/reference/check_outliers.md)
  now returns a logical vector. Original numerical vector is still
  accessible via [`as.numeric()`](https://rdrr.io/r/base/numeric.html).

### New functions

- [`pp_check()`](https://mc-stan.org/bayesplot/reference/pp_check.html)
  to compute posterior predictive checks for frequentist models.

### Bug fixes

- Fixed issue with incorrect labeling of groups from
  [`icc()`](https://easystats.github.io/performance/reference/icc.md)
  when `by_group = TRUE`.

- Fixed issue in
  [`check_heteroscedasticity()`](https://easystats.github.io/performance/reference/check_heteroscedasticity.md)
  for mixed models where sigma could not be calculated in a
  straightforward way.

- Fixed issues in
  [`check_zeroinflation()`](https://easystats.github.io/performance/reference/check_zeroinflation.md)
  for [`MASS::glm.nb()`](https://rdrr.io/pkg/MASS/man/glm.nb.html).

- Fixed CRAN check issues.

## performance 0.4.8

CRAN release: 2020-07-27

### General

- Removed suggested packages that have been removed from CRAN.

### Changes to functions

- [`icc()`](https://easystats.github.io/performance/reference/icc.md)
  now also computes a “classical” ICC for `brmsfit` models. The former
  way of calculating an “ICC” for `brmsfit` models is now available as
  new function called
  [`variance_decomposition()`](https://easystats.github.io/performance/reference/icc.md).

### Bug fixes

- Fix issue with new version of *bigutilsr* for
  [`check_outliers()`](https://easystats.github.io/performance/reference/check_outliers.md).

- Fix issue with model order in `performance_lrt()`.

## performance 0.4.7

CRAN release: 2020-06-14

### General

- Support for models from package *mfx*.

### Changes to functions

- [`model_performance.rma()`](https://easystats.github.io/performance/reference/model_performance.rma.md)
  now includes results from heterogeneity test for meta-analysis
  objects.

- [`check_normality()`](https://easystats.github.io/performance/reference/check_normality.md)
  now also works for mixed models (with the limitation that studentized
  residuals are used).

- [`check_normality()`](https://easystats.github.io/performance/reference/check_normality.md)
  gets an `effects`-argument for mixed models, to check random effects
  for normality.

### Bug fixes

- Fixed issue in
  [`performance_accuracy()`](https://easystats.github.io/performance/reference/performance_accuracy.md)
  for binomial models when response variable had non-numeric factor
  levels.

- Fixed issues in
  [`performance_roc()`](https://easystats.github.io/performance/reference/performance_roc.md),
  which printed 1 - AUC instead of AUC.

## performance 0.4.6

CRAN release: 2020-05-03

### General

- Minor revisions to
  [`model_performance()`](https://easystats.github.io/performance/reference/model_performance.md)
  to meet changes in *mlogit* package.

- Support for `bayesx` models.

### Changes to functions

- [`icc()`](https://easystats.github.io/performance/reference/icc.md)
  gains a `by_group` argument, to compute ICCs per different group
  factors in mixed models with multiple levels or cross-classified
  design.

- [`r2_nakagawa()`](https://easystats.github.io/performance/reference/r2_nakagawa.md)
  gains a `by_group` argument, to compute explained variance at
  different levels (following the variance-reduction approach by Hox
  2010).

- `performance_lrt()` now works on *lavaan* objects.

### Bug fixes

- Fix issues in some functions for models with logical dependent
  variable.

- Fix bug in
  [`check_itemscale()`](https://easystats.github.io/performance/reference/check_itemscale.md),
  which caused multiple computations of skewness statistics.

- Fix issues in
  [`r2()`](https://easystats.github.io/performance/reference/r2.md) for
  *gam* models.

## performance 0.4.5

CRAN release: 2020-03-28

### General

- [`model_performance()`](https://easystats.github.io/performance/reference/model_performance.md)
  and [`r2()`](https://easystats.github.io/performance/reference/r2.md)
  now support *rma*-objects from package *metafor*, *mlm* and *bife*
  models.

### Changes to functions

- [`compare_performance()`](https://easystats.github.io/performance/reference/compare_performance.md)
  gets a `bayesfactor` argument, to include or exclude the Bayes factor
  for model comparisons in the output.

- Added `r2.aov()`.

### Bug fixes

- Fixed issue in
  [`performance_aic()`](https://easystats.github.io/performance/reference/performance_aicc.md)
  for models from package *survey*, which returned three different AIC
  values. Now only the AIC value is returned.

- Fixed issue in
  [`check_collinearity()`](https://easystats.github.io/performance/reference/check_collinearity.md)
  for *glmmTMB* models when zero-inflated formula only had one
  predictor.

- Fixed issue in
  [`check_model()`](https://easystats.github.io/performance/reference/check_model.md)
  for *lme* models.

- Fixed issue in
  [`check_distribution()`](https://easystats.github.io/performance/reference/check_distribution.md)
  for *brmsfit* models.

- Fixed issue in
  [`check_heteroscedasticity()`](https://easystats.github.io/performance/reference/check_heteroscedasticity.md)
  for *aov* objects.

- Fixed issues for *lmrob* and *glmrob* objects.
