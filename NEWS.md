# performance 0.5.1

## Changes to functions

* `compare_performance()` with `rank = TRUE` doesn't use the `BF` values when `BIC` are present, to prevent "double-dipping" of the BIC values (#144).
* `model_performance()` now also includes the residual standard deviation.
* The `method` argument in `check_homogeneity()` gains a `"levene"` option, to use Levene's Test for homogeneity.

# performance 0.5.0

## General

* `r2()` and `icc()` support `semLME` models (package *smicd*).
* `check_heteroscedasticity()` should now also work with zero-inflated mixed models from *glmmTMB* and *GLMMadpative*.
* `check_outliers()` now returns a logical vector. Original numerical vector is still accessible via `as.numeric()`.

## New functions

* `pp_check()` to compute posterior predictive checks for frequentist models.

## Bug fixes

* Fixed issue with incorrect labeling of groups from `icc()` when `by_group = TRUE`.
* Fixed issue in `check_heteroscedasticity()` for mixed models where sigma could not be calculated in a straightforward way.
* Fixed issues in `check_zeroinflation()` for `MASS::glm.nb()`.
* Fixed CRAN check issues.

# performance 0.4.8

## General

* Removed suggested packages that have been removed from CRAN.

## Changes to functions

* `icc()` now also computes a "classical" ICC for `brmsfit` models. The former way of calculating an "ICC" for `brmsfit` models is now available as new function called `variance_decomposition()`.

## Bug fixes

* Fix issue with new version of *bigutilsr* for `check_outliers()`.
* Fix issue with model order in `performance_lrt()`.

# performance 0.4.7

## General

* Support for models from package *mfx*.

## Changes to functions

* `model_performance.rma()` now includes results from heterogeneity test for meta-analysis objects.
* `check_normality()` now also works for mixed models (with the limitation that studentized residuals are used).
* `check_normality()` gets an `effects`-argument for mixed models, to check random effects for normality.

## Bug fixes

* Fixed issue in `performance_accuracy()` for binomial models when response variable had non-numeric factor levels.
* Fixed issues in `performance_roc()`, which printed 1 - AUC instead of AUC.

# performance 0.4.6

## General

* Minor revisions to `model_performance()` to meet changes in *mlogit* package.
* Support for `bayesx` models.

## Changes to functions

* `icc()` gains a `by_group` argument, to compute ICCs per different group factors in mixed models with multiple levels or cross-classified design.
* `r2_nakagawa()` gains a `by_group` argument, to compute explained variance at different levels (following the variance-reduction approach by Hox 2010).
* `performance_lrt()` now works on *lavaan* objects.

## Bug fixes

* Fix issues in some functions for models with logical dependent variable.
* Fix bug in `check_itemscale()`, which caused multiple computations of skewness statistics.
* Fix issues in `r2()` for *gam* models.

# performance 0.4.5

## General

* `model_performance()` and `r2()` now support *rma*-objects from package *metafor*, *mlm* and *bife* models.

## Changes to functions

* `compare_performance()` gets a `bayesfactor` argument, to include or exclude the Bayes factor for model comparisons in the output.
* Added `r2.aov()`.

## Bug fixes

* Fixed issue in `performance_aic()` for models from package *survey*, which returned three different AIC values. Now only the AIC value is returned.
* Fixed issue in `check_collinearity()` for *glmmTMB* models when zero-inflated formula only had one predictor.
* Fixed issue in `check_model()` for *lme* models.
* Fixed issue in `check_distribution()` for *brmsfit* models.
* Fixed issue in `check_heteroscedasticity()` for *aov* objects.
* Fixed issues for *lmrob* and *glmrob* objects.

# performance 0.4.4

## General

* Removed `logLik.felm()`, because this method is now implemented in the *lfe* package.
* Support for `DirichletRegModel` models.

## New functions

* `check_itemscale()` to describe various measures of internal consistencies for scales which were built from several items from a PCA, using `parameters::principal_components()`.
* `r2_efron()` to compute Efron's pseudo R2.

## Bug fixes

* Fixed issue in documentation of `performance_score()`.

# performance 0.4.3

## General

* Support for `mixor`, `cpglm` and `cpglmm` models.

## New functions

* `performance_aic()` as a small wrapper that returns the AIC. It is a generic function that also works for some models that don't have a AIC method (like Tweedie models).
* `performance_lrt()` as a small wrapper around `anova()` to perform a Likelihood-Ratio-Test for model comparison.

## Bug fixes

* Fix issues with CRAN checks.

## Changes to functions

* `model_performance()` now calculates AIC for Tweedie models.

# performance 0.4.2

## General

* Support for `bracl`, `brmultinom`, `fixest`, `glmx`, `glmmadmb`, `mclogit`, `mmclogit`, `vgam` and `vglm` models.
* `model_performance()` now supports *plm* models.
* `r2()` now supports *complmrob* models.
* `compare_performance()` now gets a `plot()`-method (requires package **see**).

## Changes to functions

* `compare_performance()` gets a `rank`-argument, to rank models according to their overall model performance.
* `compare_performance()` has a nicer `print()`-method now.
* Verbosity for `compare_performance()` was slightly adjusted.
* `model_performance()`-methods for different objects now also have a `verbose`-argument.

## Minor changes

* `check_collinearity()` now no longer returns backticks in row- and column names.

## Bug fixes

* Fixed issue in `r2()` for `wbm`-models with cross-level interactions.
* `plot()`-methods for `check_heteroscedasticity()` and `check_homogeneity()` now work without requiring to load package *see* before.
* Fixed issues with models of class `rlmerMod`.

# performance 0.4.0

## General

* `performance()` is an alias for `model_performance()`.

## Deprecated and Defunct

* `principal_components()` was removed and re-implemented in the **parameters**-package. Please use `parameters::principal_components()` now.

## Changes to functions

* `check_outliers()` now also works on data frames.
* Added more methods to `check_outliers()`.
* `performance_score()` now also works on `stan_lmer()` and `stan_glmer()` objects.
* `check_singularity()` now works with models of class *clmm*.
* `r2()` now works with models of class *clmm*, *bigglm* and *biglm*.
* `check_overdispersion()` for mixed models now checks that model family is Poisson.

## Bug fixes

* Fixed bug in `compare_performance()` that toggled a warning although models were fit from same data.
* Fixed bug in `check_model()` for *glmmTMB* models that occurred when checking for outliers.

# performance 0.3.0

## General

* Many `check_*()`-methods now get a `plot()`-method. Package **see** is required for plotting.
* `model_performance()` gets a preliminary `print()`-method.

## Breaking changes

* The attribute for the standard error of the Bayesian R2 (`r2_bayes()`) was renamed from `std.error` to `SE` to be in line with the naming convention of other easystats-packages.
* `compare_performance()` now shows the Bayes factor when all compared models are fit from the same data. Previous behaviour was that the BF was shown when models were of same class.

## Changes to functions

* `model_performance()` now also works for *lavaan*-objects.
* `check_outliers()` gets a `method`-argument to choose the method for detecting outliers. Furthermore, two new methods (Mahalanobis Distance and Invariant Coordinate Selection) were implemented.
* `check_model()` now performs more checks for GLM(M)s and other model objects.
* `check_model()` gets a `check`-argument to plot selected checks only.
* `r2_nakagawa()` now returns r-squared for models with singular fit, where no random effect variances could be computed. The r-squared then does not take random effect variances into account. This behaviour was changed to be in line with `MuMIn::r.squaredGLMM()`, which returned a value for models with singular fit.
* `check_distribution()` now detects negative binomial and zero-inflated distributions. Furthermore, attempt to improve accuracy.
* `check_distribution()` now also accepts a numeric vector as input.
* `compare_performance()` warns if models were not fit from same data.

## New check-functions

* `check_homogeneity()` to check models for homogeneity of variances.

## Bug fixes

* Fixed issues with `compare_performance()` and row-ordering.
* Fixed issue in `check_collinearity()` for zero-inflated models, where the zero-inflation component had not enough model terms to calculate multicollinearity.
* Fixed issue in some `check_*()` and `performance_*()` functions for models with binary outcome, when outcome variable was a factor.

# performance 0.2.0

## General

* `r2()` now works for more regression models.
* `r2_bayes()` now works for multivariate response models.
* `model_performance()` now works for more regression models, and also includes the log-loss, proper scoring rules and percentage of correct predictions as new metric for models with binary outcome.

## New performance-functions

* `performance_accuracy()`, which calculates the predictive accuracy of linear or logistic regression models.
* `performance_logloss()` to compute the log-loss of models with binary outcome. The log-loss is a proper scoring function comparable to the `rmse()`.
* `performance_score()` to compute the logarithmic, quadratic and spherical proper scoring rules.
* `performance_pcp()` to calculate the percentage of correct predictions for models with binary outcome.
* `performance_roc()`, to calculate ROC-curves.
* `performance_aicc()`, to calculate the second-order AIC (AICc).

## New check-functions

* `check_collinearity()` to calculate the variance inflation factor and check model predictors for multicollinearity.
* `check_outliers()` to check models for influential observations.
* `check_heteroscedasticity()` to check models for (non-)constant error variance.
* `check_normality()` to check models for (non-)normality of residuals.
* `check_autocorrelation()` to check models for auto-correlated residuals.
* `check_distribution()` to classify the distribution of a model-family using machine learning.

## New indices-functions

* `r2_mckelvey()` to compute McKelvey and Zavoinas R2 value.
* `r2_zeroinflated()` to compute R2 for zero-inflated (non-mixed) models.
* `r2_xu()` as a crude R2 measure for linear (mixed) models.

## Breaking changes

* `model_performance.stanreg()` and `model_performance.brmsfit()` now only return one R2-value and its standard error, instead of different (robust) R2 measures and credible intervals.
* `error_rate()` is now integrated in the `performance_pcp()`-function.

## Changes to functions

* `model_performance.stanreg()` and `model_performance.brmsfit()` now also return the _WAIC_ (widely applicable information criterion).
* `r2_nakagawa()` now calculates the full R2 for mixed models with zero-inflation.
* `icc()` now returns `NULL` and no longer stops when no mixed model is provided.
* `compare_performance()` now shows the Bayes factor when all compared models are of same class.
* Some functions get a `verbose`-argument to show or suppress warnings.

## Bug fixes

* Renamed `r2_coxnell()` to `r2_coxsnell()`.
* Fix issues in `r2_bayes()` and `model_performance()` for ordinal models resp. models with cumulative link (#48).
* `compare_performance()` did not sort the `name`-column properly, if the columns `class` and `name` were not in the same alphabetical order (#51).
