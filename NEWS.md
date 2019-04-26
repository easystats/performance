# performance 0.1.1

## General

* `r2()` now works for more regression models.
* `r2_bayes()` now works for multivariate response models.
* `model_performance()` now works for more regression models, and also includes the log-loss as new metric for models with binary outcome.

## New functions

* `log_loss()` to compute the log-loss of models with binary outcome. The log-loss is a proper scoring function comparable to the `rmse()`.

## Bug fixes

* Renamed `r2_coxnell()` to `r2_coxsnell()`.
