# performance 0.14.0

## Breaking Changes

* The `"Increased SE"` column in the output of `check_collinearity()` was renamed
  into `"adj. VIF"` (=adjusted VIF). Furthermore, the computation of the adjusted
  VIF now correctly accounts for the numbers of levels (i.e. degrees of freedom)
  for factors.

## New functions

* New function `check_group_variation()` to check within-/between-group
  variability (this function will replace `check_heterogeneity_bias()` in
  future releases.)

* New functions `performance_reliability()` and `performance_dvour()`. These
  functions provide information about the reliability of group-level estimates
  (i.e., random effects) in mixed models.

## Changes

* Singularity checks with `check_singularity()` are now more efficient and also
  include the random effects for the dispersion component (from package
  *glmmTMB*). Furthermore, a `check` argument allows to check for general
  singularity (for the full model), or can return singularity checks for each
  random effects term separately.

## Bug fixes

* Fixed issue with wrong computation of pseudo-R2 for some models where the
  base-model (null model) was updated using the original data, which could
  include missing values. Now the model frame is used, ensuring the correct
  number of observations in the returned base-model, thus calculating the
  correct log-likelihood and returning the correct pseudo-R2.

* Fixed examples in `check_outliers()`.

# performance 0.13.0

## Breaking changes

* `check_outliers()` with `method = "optics"` now returns a further refined
  cluster selection, by passing the `optics_xi` argument to
  `dbscan::extractXi()`.

* Deprecated arguments and alias-function-names have been removed.

* Argument names in `check_model()` that refer to plot-aesthetics (like
  `dot_size`) are now harmonized across *easystats* packages, meaning that
  these have been renamed. They now follow the pattern `aesthetic_type`, e.g.
  `size_dot` (instead of `dot_size`).

## Changes

* Increased accuracy for `check_convergence()` for *glmmTMB* models.

* `r2()` and `r2_mcfadden()` now support beta-binomial (non-mixed) models from
  package *glmmTMB*.

* An `as.numeric()` resp. `as.double()` method for objects of class
  `performance_roc` was added.

* Improved documentation for `performance_roc()`.

## Bug fixes

* `check_outliers()` did not warn that no numeric variables were found when only
  the response variable was numeric, but all relevant predictors were not.

* `check_collinearity()` did not work for glmmTMB models when zero-inflation
  component was set to `~0`.

# performance 0.12.4

## Changes

* `check_dag()` now also checks for colliders, and suggests removing it in the
  printed output.

* Minor revisions to the printed output of `check_dag()`.

## Bug fixes

* Fixed failing tests that broke due to changes in latest *glmmTMB* update.

# performance 0.12.3

## New functions

* `check_dag()`, to check DAGs for correct adjustment sets.

## Changes

* `check_heterogeneity_bias()` gets a `nested` argument. Furthermore, `by` can
  specify more than one variable, meaning that nested or cross-classified
  model designs can also be tested for heterogeneity bias.

# performance 0.12.2

Patch release, to ensure that _performance_ runs with older version of
_datawizard_ on Mac OSX with R (old-release).

# performance 0.12.1

## General

* `icc()` and `r2_nakagawa()` get a `null_model` argument. This can be useful
  when computing R2 or ICC for mixed models, where the internal computation of
  the null model fails, or when you already have fit the null model and want
  to save time.

* `icc()` and `r2_nakagawa()` get a `approximation` argument indicating the
  approximation method for the distribution-specific (residual) variance. See
  Nakagawa et al. 2017 for details.

* `icc()` and `r2_nakagawa()` get a `model_component` argument indicating the
  component for zero-inflation or hurdle models.

* `performance_rmse()` (resp. `rmse()`) can now compute analytical and
  bootstrapped confidence intervals. The function gains following new arguments:
  `ci`, `ci_method` and `iterations`.

* New function `r2_ferrari()` to compute Ferrari & Cribari-Neto's R2 for
  generalized linear models, in particular beta-regression.

* Improved documentation of some functions.

## Bug fixes

* Fixed issue in `check_model()` when model contained a transformed response
  variable that was named like a valid R function name (e.g., `lm(log(lapply) ~ x)`,
  when data contained a variable named `lapply`).

* Fixed issue in `check_predictions()` for linear models when response was
  transformed as ratio (e.g. `lm(succes/trials ~ x)`).

* Fixed issue in `r2_bayes()` for mixed models from *rstanarm*.

# performance 0.12.0

## Breaking

* Aliases `posterior_predictive_check()` and `check_posterior_predictions()` for
  `check_predictions()` are deprecated.

* Arguments named `group` or `group_by` will be deprecated in a future release.
  Please use `by` instead. This affects `check_heterogeneity_bias()` in
  *performance*.

## General

* Improved documentation and new vignettes added.

* `check_model()` gets a `base_size` argument, to set the base font size for plots.

* `check_predictions()` for `stanreg` and `brmsfit` models now returns plots in
  the usual style as for other models and no longer returns plots from
  `bayesplot::pp_check()`.

* Updated the trained model that is used to prediction distributions in
  `check_distribution()`.

## Bug fixes

* `check_model()` now falls back on normal Q-Q plots when a model is not supported
  by the DHARMa package and simulated residuals cannot be calculated.

# performance 0.11.0

## New supported models

* Rudimentary support for models of class `serp` from package *serp*.

## New functions

* `simulate_residuals()` and `check_residuals()`, to simulate and check residuals
  from generalized linear (mixed) models. Simulating residuals is based on the
  DHARMa package, and objects returned by `simulate_residuals()` inherit from
  the `DHARMa` class, and thus can be used with any functions from the *DHARMa*
  package. However, there are also implementations in the *performance* package,
  such as `check_overdispersion()`, `check_zeroinflation()`, `check_outliers()`
  or `check_model()`.

* Plots for `check_model()` have been improved. The Q-Q plots are now based
  on simulated residuals from the DHARMa package for non-Gaussian models, thus
  providing more accurate and informative plots. The half-normal QQ plot for
  generalized linear models can still be obtained by setting the new argument
  `residual_type = "normal"`.

* Following functions now support simulated residuals (from `simulate_residuals()`)
  resp. objects returned from `DHARMa::simulateResiduals()`:
  - `check_overdispersion()`
  - `check_zeroinflation()`
  - `check_outliers()`
  - `check_model()`

## General

* Improved error messages for `check_model()` when QQ-plots cannot be created.

* `check_distribution()` is more stable for possibly sparse data.

## Bug fixes

* Fixed issue in `check_normality()` for t-tests.

* Fixed issue in `check_itemscale()` for data frame inputs, when `factor_index`
  was not a named vector.

# performance 0.10.9

## Changes

* `r2()` for models of class `glmmTMB` without random effects now returns the
  correct r-squared value for non-mixed models.

* `check_itemscale()` now also accepts data frames as input. In this case,
  `factor_index` must be specified, which must be a numeric vector of same
  length as number of columns in `x`, where each element is the index of the
  factor to which the respective column in `x`.

* `check_itemscale()` gets a `print_html()` method.

* Clarification in the documentation of the `estimator` argument for
  `performance_aic()`.

* Improved plots for overdispersion-checks for negative-binomial models from
  package *glmmTMB* (affects `check_overdispersion()` and `check_model()`).

* Improved detection rates for singularity in `check_singularity()` for models
  from package *glmmTMB*.

* For model of class `glmmTMB`, deviance residuals are now used in the
  `check_model()` plot.

* Improved (better to understand) error messages for `check_model()`,
  `check_collinearity()` and `check_outliers()` for models with non-numeric
  response variables.

* `r2_kullback()` now gives an informative error for non-supported models.

## Bug fixes

* Fixed issue in `binned_residuals()` for models with binary outcome, where
  in rare occasions empty bins could occur.

* `performance_score()` should no longer fail for models where scoring rules
  can't be calculated. Instead, an informative message is returned.

* `check_outliers()` now properly accept the `percentage_central` argument when
  using the `"mcd"` method.

* Fixed edge cases in `check_collinearity()` and `check_outliers()` for models
  with response variables of classes `Date`, `POSIXct`, `POSIXlt` or `difftime`.

* Fixed issue with `check_model()` for models of package *quantreg*.

# performance 0.10.8

## Changes

* Changed behaviour of `check_predictions()` for models from binomial family,
  to get comparable plots for different ways of outcome specification. Now,
  if the outcome is a proportion, or defined as matrix of trials and successes,
  the produced plots are the same (because the models should be the same, too).

## Bug fixes

* Fixed CRAN check errors.

* Fixed issue with `binned_residuals()` for models with binomial family, where
  the outcome was a proportion.

# performance 0.10.7

## Breaking changes

* `binned_residuals()` gains a few new arguments to control the residuals used
  for the test, as well as different options to calculate confidence intervals
  (namely, `ci_type`, `residuals`, `ci` and `iterations`). The default values
  to compute binned residuals have changed. Default residuals are now "deviance"
  residuals (and no longer "response" residuals). Default confidence intervals
  are now "exact" intervals (and no longer based on Gaussian approximation).
  Use `ci_type = "gaussian"` and `residuals = "response"` to get the old defaults.

## Changes to functions

* `binned_residuals()` - like `check_model()` - gains a `show_dots` argument to
  show or hide data points that lie inside error bounds. This is particular
  useful for models with many observations, where generating the plot would be
  very slow.

# performance 0.10.6

## General

* Support for `nestedLogit` models.

## Changes to functions

* `check_outliers()` for method `"ics"` now detects number of available cores
  for parallel computing via the `"mc.cores"` option. This is more robust than
  the previous method, which used `parallel::detectCores()`. Now you should
  set the number of cores via `options(mc.cores = 4)`.

## Bug fixes

* Fixed issues is `check_model()` for models that used data sets with
  variables of class `"haven_labelled"`.

# performance 0.10.5

## Changes to functions

* More informative message for `test_*()` functions that "nesting" only refers
  to fixed effects parameters and currently ignores random effects when detecting
  nested models.

* `check_outliers()` for `"ICS"` method is now more stable and less likely to
  fail.

* `check_convergence()` now works for *parsnip* `_glm` models.

## Bug fixes

* `check_collinearity()` did not work for hurdle- or zero-inflated models of
  package *pscl* when model had no explicitly defined formula for the
  zero-inflation model.

# performance 0.10.4

## Changes to functions

* `icc()` and `r2_nakagawa()` gain a `ci_method` argument, to either calculate
  confidence intervals using `boot::boot()` (instead of `lmer::bootMer()`) when
  `ci_method = "boot"` or analytical confidence intervals
  (`ci_method = "analytical"`). Use `ci_method = "boot"` when the default method
  fails to compute confidence intervals and use `ci_method = "analytical"` if
  bootstrapped intervals cannot be calculated at all. Note that the default
  computation method is preferred.

* `check_predictions()` accepts a `bandwidth` argument (smoothing bandwidth),
  which is passed down to the `plot()` methods density-estimation.

* `check_predictions()` gains a `type` argument, which is passed down to the
  `plot()` method to change plot-type (density or discrete dots/intervals).
  By default, `type` is set to `"default"` for models without discrete outcomes,
  and else `type = "discrete_interval"`.

* `performance_accuracy()` now includes confidence intervals, and reports those
  by default (the standard error is no longer reported, but still included).

## Bug fixes

* Fixed issue in `check_collinearity()` for _fixest_ models that used `i()`
  to create interactions in formulas.

# performance 0.10.3

## New functions

* `item_discrimination()`, to calculate the discrimination of a scale's items.

## Support for new models

* `model_performance()`, `check_overdispersion()`, `check_outliers()` and `r2()`
  now work with objects of class `fixest_multi` (@etiennebacher, #554).

* `model_performance()` can now return the "Weak instruments" statistic and
  p-value for models of class `ivreg` with `metrics = "weak_instruments"`
  (@etiennebacher, #560).

* Support for `mclogit` models.

## Changes to functions

* `test_*()` functions now automatically fit a null-model when only one model
  objects was provided for testing multiple models.

* Warnings in `model_performance()` for unsupported objects of class
  `BFBayesFactor` can now be suppressed with `verbose = FALSE`.

* `check_predictions()` no longer fails with issues when `re_formula = NULL`
  for mixed models, but instead gives a warning and tries to compute posterior
  predictive checks with `re_formuka = NA`.

* `check_outliers()` now also works for meta-analysis models from packages
  *metafor* and *meta*.

* `plot()` for `performance::check_model()` no longer produces a normal QQ plot
  for GLMs. Instead, it now shows a half-normal QQ plot of the absolute value
  of the standardized deviance residuals.

## Bug fixes

* Fixed issue in `print()` method for `check_collinearity()`, which could mix
  up the correct order of parameters.

# performance 0.10.2

## General

* Revised usage of `insight::get_data()` to meet forthcoming changes in the
  _insight_ package.

## Changes to functions

* `check_collinearity()` now accepts `NULL` for the `ci` argument.

## Bug fixes

* Fixed issue in `item_difficulty()` with detecting the maximum values of an
  item set. Furthermore, `item_difficulty()` gets a `maximum_value` argument
  in case no item contains the maximum value due to missings.

# performance 0.10.1

## General

* Minor improvements to the documentation.

## Changes to functions

* `icc()` and `r2_nakagawa()` get `ci` and `iterations` arguments, to compute
  confidence intervals for the ICC resp. R2, based on bootstrapped sampling.

* `r2()` gets `ci`, to compute (analytical) confidence intervals for the R2.

* The model underlying `check_distribution()` was now also trained to detect
  cauchy, half-cauchy and inverse-gamma distributions.

* `model_performance()` now allows to include the ICC for Bayesian models.

## Bug fixes

* `verbose` didn't work for `r2_bayes()` with `BFBayesFactor` objects.

* Fixed issues in `check_model()` for models with convergence issues that lead
  to `NA` values in residuals.

* Fixed bug in `check_outliers` whereby passing multiple elements to the
  threshold list generated an error (#496).

* `test_wald()` now warns the user about inappropriate F test and calls
  `test_likelihoodratio()` for binomial models.

* Fixed edge case for usage of `parellel::detectCores()` in `check_outliers()`.

# performance 0.10.0

## Breaking Change

* The minimum needed R version has been bumped to `3.6`.

* The alias `performance_lrt()` was removed. Use `test_lrt()` resp.
  `test_likelihoodratio()`.

## New functions

* Following functions were moved from package *parameters* to *performance*:
  `check_sphericity_bartlett()`, `check_kmo()`, `check_factorstructure()` and
  `check_clusterstructure()`.

## Changes to functions

* `check_normality()`, `check_homogeneity()` and `check_symmetry()` now works
  for `htest` objects.

* Print method for `check_outliers()` changed significantly: now states the
  methods, thresholds, and variables used, reports outliers per variable (for
  univariate methods) as well as any observation flagged for several
  variables/methods. Includes a new optional ID argument to add along the
  row number in the output (@rempsyc #443).

* `check_outliers()` now uses more conventional outlier thresholds. The `IQR`
  and confidence interval methods now gain improved distance scores that
  are continuous instead of discrete.

## Bug Fixes

* Fixed wrong *z*-score values when using a vector instead of a data frame in
  `check_outliers()` (#476).

* Fixed `cronbachs_alpha()` for objects from `parameters::principal_component()`.

# performance 0.9.2

## General

* `print()` methods for `model_performance()` and `compare_performance()` get a
  `layout` argument, which can be `"horizontal"` (default) or `"vertical"`, to
  switch the layout of the printed table.

* Improved speed performance for `check_model()` and some other
  `performance_*()` functions.

* Improved support for models of class `geeglm`.

## Changes to functions

* `check_model()` gains a `show_dots` argument, to show or hide data points.
  This is particular useful for models with many observations, where generating
  the plot would be very slow.

## Bug Fixes

* Fixes wrong column names in `model_performance()` output for `kmeans` objects
  (#453)

# performance 0.9.1

## Breaking

* The formerly "conditional" ICC in `icc()` is now named "unadjusted" ICC.

## New functions

* `performance_cv()` for cross-validated model performance.

## Support for new models

* Added support for models from package *estimator*.

## Changes to functions

* `check_overdispersion()` gets a `plot()` method.

* `check_outliers()` now also works for models of classes `gls` and `lme`. As a
  consequence, `check_model()` will no longer fail for these models.

* `check_collinearity()` now includes the confidence intervals for the VIFs and
  tolerance values.

* `model_performance()` now also includes within-subject R2 measures, where
  applicable.

* Improved handling of random effects in `check_normality()` (i.e. when argument
  `effects = "random"`).

## Bug fixes

* `check_predictions()` did not work for GLMs with matrix-response.

* `check_predictions()` did not work for logistic regression models (i.e. models
  with binary response) from package *glmmTMB*

* `item_split_half()` did not work when the input data frame or matrix only
  contained two columns.

* Fixed wrong computation of `BIC` in `model_performance()` when models had
  transformed response values.

* Fixed issues in `check_model()` for GLMs with matrix-response.

# performance 0.9.0

## New functions

* `check_concurvity()`, which returns GAM concurvity measures (comparable to
  collinearity checks).

## Changes to functions

### Check functions

* `check_predictions()`, `check_collinearity()` and `check_outliers()` now
  support (mixed) regression models from `BayesFactor`.

* `check_zeroinflation()` now also works for `lme4::glmer.nb()` models.

* `check_collinearity()` better supports GAM models.

### Test functions

* `test_performance()` now calls `test_lrt()` or `test_wald()` instead of
  `test_vuong()` when package *CompQuadForm* is missing.

* `test_performance()` and `test_lrt()` now compute the corrected log-likelihood
  when models with transformed response variables (such as log- or
  sqrt-transformations) are passed to the functions.

### Model performance functions

* `performance_aic()` now corrects the AIC value for models with transformed
  response variables. This also means that comparing models using
  `compare_performance()` allows comparisons of AIC values for models with and
  without transformed response variables.

* Also, `model_performance()` now corrects both AIC and BIC values for models
  with transformed response variables.

### Plotting and printing

* The `print()` method for `binned_residuals()` now prints a short summary of
  the results (and no longer generates a plot). A `plot()` method was added to
  generate plots.

* The `plot()` output for `check_model()` was revised:

  - For binomial models, the constant variance plot was omitted, and a binned
    residuals plot included.

  - The density-plot that showed normality of residuals was replaced by the
    posterior predictive check plot.

## Bug fixes

* `model_performance()` for models from *lme4* did not report AICc when
  requested.

* `r2_nakagawa()` messed up order of group levels when `by_group` was `TRUE`.

# performance 0.8.0

## Breaking Changes

* The `ci`-level in `r2()` for Bayesian models now defaults to `0.95`, to be in
  line with the latest changes in the *bayestestR* package.

* S3-method dispatch for `pp_check()` was revised, to avoid problems with the
  _bayesplot_ package, where the generic is located.

## General

* Minor revisions to wording for messages from some of the check-functions.

* `posterior_predictive_check()` and `check_predictions()` were added as aliases
  for `pp_check()`.

## New functions

* `check_multimodal()` and `check_heterogeneity_bias()`. These functions will be
  removed from the _parameters_ packages in the future.

## Changes to functions

* `r2()` for linear models can now compute confidence intervals, via the `ci`
  argument.

## Bug fixes

* Fixed issues in `check_model()` for Bayesian models.

* Fixed issue in `pp_check()` for models with transformed response variables, so
  now predictions and observed response values are on the same (transformed)
  scale.

# performance 0.7.3

## Changes to functions

* `check_outliers()` has new `ci` (or `hdi`, `eti`) method to filter based on
  Confidence/Credible intervals.

* `compare_performance()` now also accepts a list of model objects.

* `performance_roc()` now also works for binomial models from other classes than
  *glm*.

* Several functions, like `icc()` or `r2_nakagawa()`, now have an
  `as.data.frame()` method.

* `check_collinearity()` now correctly handles objects from forthcoming *afex*
  update.

# performance 0.7.2

## New functions

* `performance_mae()` to calculate the mean absolute error.

## Bug fixes

* Fixed issue with `"data length differs from size of matrix"` warnings in
  examples in forthcoming R 4.2.

* Fixed issue in `check_normality()` for models with sample size larger than

5.000 observations.

* Fixed issue in `check_model()` for *glmmTMB* models.

* Fixed issue in `check_collinearity()` for *glmmTMB* models with
  zero-inflation, where the zero-inflated model was an intercept-only model.

# performance 0.7.1

## New supported models

* Add support for `model_fit` (*tidymodels*).

* `model_performance` supports *kmeans* models.

## General

* Give more informative warning when `r2_bayes()` for *BFBayesFactor* objects
  can't be calculated.

* Several `check_*()` functions now return informative messages for invalid
  model types as input.

* `r2()` supports `mhurdle` (*mhurdle*) models.

* Added `print()` methods for more classes of `r2()`.

* The `performance_roc()` and `performance_accuracy()` functions unfortunately
  had spelling mistakes in the output columns: *Sensitivity* was called
  *Sensivity* and *Specificity* was called *Specifity*. We think these are
  understandable mistakes :-)

## Changes to functions

### `check_model()`

* `check_model()` gains more arguments, to customize plot appearance.

* Added option to detrend QQ/PP plots in `check_model()`.

### `model_performance()`

* The `metrics` argument from `model_performance()` and `compare_performance()`
  gains a `"AICc"` option, to also compute the 2nd order AIC.

* `"R2_adj"` is now an explicit option in the `metrics` argument from
  `model_performance()` and `compare_performance()`.

### Other functions

* The default-method for `r2()` now tries to compute an r-squared for all models
  that have no specific `r2()`-method yet, by using following formula:
  `1-sum((y-y_hat)^2)/sum((y-y_bar)^2))`

* The column name `Parameter` in `check_collinearity()` is now more
  appropriately named `Term`.

## Bug fixes

* `test_likelihoodratio()` now correctly sorts models with identical fixed
  effects part, but different other model parts (like zero-inflation).

* Fixed incorrect computation of models from inverse-Gaussian families, or
  Gaussian families fitted with `glm()`.

* Fixed issue in `performance_roc()` for models where outcome was not 0/1
  coded.

* Fixed issue in `performance_accuracy()` for logistic regression models when
  `method = "boot"`.

* `cronbachs_alpha()` did not work for `matrix`-objects, as stated in the docs.
  It now does.

# performance 0.7.0

## General

* Roll-back R dependency to R >= 3.4.

## Breaking Changes

* `compare_performance()` doesn't return the models' Bayes Factors, now returned
  by `test_performance()` and `test_bf()`.

## New functions to test or compare models

* `test_vuong()`, to compare models using Vuong's (1989) Test.

* `test_bf()`, to compare models using Bayes factors.

* `test_likelihoodratio()` as an alias for `performance_lrt()`.

* `test_wald()`, as a rough approximation for the LRT.

* `test_performance()`, to run the most relevant and appropriate tests based on
  the input.

## Changes to functions

### `performance_lrt()`

* `performance_lrt()` get an alias `test_likelihoodratio()`.

* Does not return AIC/BIC now (as they are not related to LRT *per se* and can
  be easily obtained with other functions).

* Now contains a column with the difference in degrees of freedom between
  models.

* Fixed column names for consistency.

### `model_performance()`

* Added more diagnostics to models of class `ivreg`.

### Other functions

* Revised computation of `performance_mse()`, to ensure that it's always based
  on response residuals.

* `performance_aic()` is now more robust.

## Bug fixes

* Fixed issue in `icc()` and `variance_decomposition()` for multivariate
  response models, where not all model parts contained random effects.

* Fixed issue in `compare_performance()` with duplicated rows.

* `check_collinearity()` no longer breaks for models with rank deficient model
  matrix, but gives a warning instead.

* Fixed issue in `check_homogeneity()` for `method = "auto"`, which wrongly
  tested the response variable, not the residuals.

* Fixed issue in `check_homogeneity()` for edge cases where predictor had
  non-syntactic names.

# performance 0.6.1

## General

* `check_collinearity()` gains a `verbose` argument, to toggle warnings and
  messages.

## Bug fixes

* Fixed examples, now using suggested packages only conditionally.

# performance 0.6.0

## General

* `model_performance()` now supports `margins`, `gamlss`, `stanmvreg` and
  `semLme`.

## New functions

* `r2_somers()`, to compute Somers' Dxy rank-correlation as R2-measure for
  logistic regression models.

* `display()`, to print output from package-functions into different formats.
  `print_md()` is an alias for `display(format = "markdown")`.

## Changes to functions

### `model_performance()`

* `model_performance()` is now more robust and doesn't fail if an index could
  not be computed. Instead, it returns all indices that were possible to
  calculate.

* `model_performance()` gains a default-method that catches all model objects
  not previously supported. If model object is also not supported by the
  default-method, a warning is given.

* `model_performance()` for metafor-models now includes the degrees of freedom
  for Cochran's Q.

### Other functions

* `performance_mse()` and `performance_rmse()` now always try to return the
  (R)MSE on the response scale.

* `performance_accuracy()` now accepts all types of linear or logistic
  regression models, even if these are not of class `lm` or `glm`.

* `performance_roc()` now accepts all types of logistic regression models, even
  if these are not of class `glm`.

* `r2()` for mixed models and `r2_nakagawa()` gain a `tolerance`-argument, to
  set the tolerance level for singularity checks when computing random effect
  variances for the conditional r-squared.

## Bug fixes

* Fixed issue in `icc()` introduced in the last update that make `lme`-models
  fail.

* Fixed issue in `performance_roc()` for models with factors as response.

# performance 0.5.1

## Breaking changes

* Column names for `model_performance()` and `compare_performance()` were
  changed to be in line with the _easystats_ naming convention: `LOGLOSS` is now
  `Log_loss`, `SCORE_LOG` is `Score_log` and `SCORE_SPHERICAL` is now
  `Score_spherical`.

## New functions
* `r2_posterior()` for Bayesian models to obtain posterior distributions of
  R-squared.

## Changes to functions

* `r2_bayes()` works with Bayesian models from `BayesFactor` ( #143 ).

* `model_performance()` works with Bayesian models from `BayesFactor` ( #150 ).

* `model_performance()` now also includes the residual standard deviation.

* Improved formatting for Bayes factors in `compare_performance()`.

* `compare_performance()` with `rank = TRUE` doesn't use the `BF` values when
  `BIC` are present, to prevent "double-dipping" of the BIC values (#144).

* The `method` argument in `check_homogeneity()` gains a `"levene"` option, to
  use Levene's Test for homogeneity.

## Bug fixes

* Fix bug in `compare_performance()` when `...` arguments were function calls to
  regression objects, instead of direct function calls.

# performance 0.5.0

## General

* `r2()` and `icc()` support `semLME` models (package *smicd*).

* `check_heteroscedasticity()` should now also work with zero-inflated mixed
  models from *glmmTMB* and *GLMMadpative*.

* `check_outliers()` now returns a logical vector. Original numerical vector is
  still accessible via `as.numeric()`.

## New functions

* `pp_check()` to compute posterior predictive checks for frequentist models.

## Bug fixes

* Fixed issue with incorrect labeling of groups from `icc()` when `by_group =
  TRUE`.

* Fixed issue in `check_heteroscedasticity()` for mixed models where sigma could
  not be calculated in a straightforward way.

* Fixed issues in `check_zeroinflation()` for `MASS::glm.nb()`.

* Fixed CRAN check issues.

# performance 0.4.8

## General

* Removed suggested packages that have been removed from CRAN.

## Changes to functions

* `icc()` now also computes a "classical" ICC for `brmsfit` models. The former
  way of calculating an "ICC" for `brmsfit` models is now available as new
  function called `variance_decomposition()`.

## Bug fixes

* Fix issue with new version of *bigutilsr* for `check_outliers()`.

* Fix issue with model order in `performance_lrt()`.

# performance 0.4.7

## General

* Support for models from package *mfx*.

## Changes to functions

* `model_performance.rma()` now includes results from heterogeneity test for
  meta-analysis objects.

* `check_normality()` now also works for mixed models (with the limitation that
  studentized residuals are used).

* `check_normality()` gets an `effects`-argument for mixed models, to check
  random effects for normality.

## Bug fixes

* Fixed issue in `performance_accuracy()` for binomial models when response
  variable had non-numeric factor levels.

* Fixed issues in `performance_roc()`, which printed 1 - AUC instead of AUC.

# performance 0.4.6

## General

* Minor revisions to `model_performance()` to meet changes in *mlogit* package.

* Support for `bayesx` models.

## Changes to functions

* `icc()` gains a `by_group` argument, to compute ICCs per different group
  factors in mixed models with multiple levels or cross-classified design.

* `r2_nakagawa()` gains a `by_group` argument, to compute explained variance at
  different levels (following the variance-reduction approach by Hox 2010).

* `performance_lrt()` now works on *lavaan* objects.

## Bug fixes

* Fix issues in some functions for models with logical dependent variable.

* Fix bug in `check_itemscale()`, which caused multiple computations of skewness
  statistics.

* Fix issues in `r2()` for *gam* models.

# performance 0.4.5

## General

* `model_performance()` and `r2()` now support *rma*-objects from package
  *metafor*, *mlm* and *bife* models.

## Changes to functions

* `compare_performance()` gets a `bayesfactor` argument, to include or exclude
  the Bayes factor for model comparisons in the output.

* Added `r2.aov()`.

## Bug fixes

* Fixed issue in `performance_aic()` for models from package *survey*, which
  returned three different AIC values. Now only the AIC value is returned.

* Fixed issue in `check_collinearity()` for *glmmTMB* models when zero-inflated
  formula only had one predictor.

* Fixed issue in `check_model()` for *lme* models.

* Fixed issue in `check_distribution()` for *brmsfit* models.

* Fixed issue in `check_heteroscedasticity()` for *aov* objects.

* Fixed issues for *lmrob* and *glmrob* objects.
