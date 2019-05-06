# performance 0.1.1

## General

* `r2()` now works for more regression models.
* `r2_bayes()` now works for multivariate response models.
* `model_performance()` now works for more regression models, and also includes the log-loss and percentage of correct predictions as new metric for models with binary outcome.

## New functions

* `performance_accuracy()`, which calculates the predictive accuracy of linear or logistic regression models.
* `performance_logloss()` to compute the log-loss of models with binary outcome. The log-loss is a proper scoring function comparable to the `rmse()`.
* `performance_pcp()` to calculate the percentage of correct predictions for models with binary outcome.
* `r2_mckelvey()` to compute McKelvey and Zavoinas R2 value.
* `r2_zeroinflated()` as a crude R2 measure for zero-inflated models.
* `r2_xu()` as a crude R2 measure for linear (mixed) models.

## Breaking changes

* `model_performance.stanreg()` and `model_performance.brmsfit()` now only return one R2-value and its standard error, instead of different (robust) R2 measures and credible intervals.
* `error_rate()` is now integrated in the `performance_pcp()`-function.

## Changes to functions

* `model_performance.stanreg()` and `model_performance.brmsfit()` now also return the _WAIC_ (widely applicable information criterion).

## Bug fixes

* Renamed `r2_coxnell()` to `r2_coxsnell()`.
* Fix issues in `r2_bayes()` and `model_performance()` for ordinal models resp. models with cumulative link (#48).
