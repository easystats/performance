# performance 0.1.1

## General

* `r2()` now works for more regression models.
* `r2_bayes()` now works for multivariate response models.
* `model_performance()` now works for more regression models, and also includes the log-loss as new metric for models with binary outcome.

## New functions

* `performance_accuracy()`, which calculates the predictive accuracy of linear or logistic regression models.
* `performance_logloss()` to compute the log-loss of models with binary outcome. The log-loss is a proper scoring function comparable to the `rmse()`.
* `performance_pcp()` to calculate the percentage of correct predictions for models with binary outcome.

## Breaking changes

* `model_performance.stanreg()` and `model_performance.brmsfit()` now only return one R2-value and its standard error, instead of different (robust) R2 measures and credible intervals.

## Bug fixes

* Renamed `r2_coxnell()` to `r2_coxsnell()`.
