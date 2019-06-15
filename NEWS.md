# performance 0.2.1

## General

* Many `check_*()`-methods now get a `plot()`-method. Package **see** is required for plotting.

## Changes to functions

* `check_outliers()` gets a `method`-argument to choose the method for detecting outliers. Furthermore, two new methods (Mahalanobis Distance and Invariant Coordinate Selection) were implemented.
* `check_model()` now performs more checks for GLM(M)s and other model objects.
* `r2_nakagawa()` now returns r-squared for models with singular fit, where no random effect variances could be computed. The r-squared then does not take random effect variances into account. This behaviour was changed to be in line with `MuMIn::r.squaredGLMM()`, which returned a value for models with singular fit.
* `check_distribution()` now detects negative binomial and zero-inflated distributions. Furthermore, attempt to improve accuracy.

## Bug fixes

* Fix issues with `compare_performance()` and row-ordering.

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
* Some functions get a `verbose`-argument to sow or suppress warnings.

## Bug fixes

* Renamed `r2_coxnell()` to `r2_coxsnell()`.
* Fix issues in `r2_bayes()` and `model_performance()` for ordinal models resp. models with cumulative link (#48).
* `compare_performance()` did not sort the `name`-column properly, if the columns `class` and `name` were not in the same alphabetical order (#51).