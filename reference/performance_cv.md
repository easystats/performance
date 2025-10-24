# Cross-validated model performance

This function cross-validates regression models in a user-supplied new
sample or by using holdout (train-test), k-fold, or leave-one-out
cross-validation.

## Usage

``` r
performance_cv(
  model,
  data = NULL,
  method = "holdout",
  metrics = "all",
  prop = 0.3,
  k = 5,
  stack = TRUE,
  verbose = TRUE,
  ...
)
```

## Arguments

- model:

  A regression model.

- data:

  Optional. A data frame containing the same variables as `model` that
  will be used as the cross-validation sample.

- method:

  Character string, indicating the cross-validation method to use:
  whether holdout (`"holdout"`, aka train-test), k-fold (`"k_fold"`), or
  leave-one-out (`"loo"`). If `data` is supplied, this argument is
  ignored.

- metrics:

  Can be `"all"`, `"common"` or a character vector of metrics to be
  computed (some of `c("ELPD", "Deviance", "MSE", "RMSE", "R2")`).
  "common" will compute R2 and RMSE.

- prop:

  If `method = "holdout"`, what proportion of the sample to hold out as
  the test sample?

- k:

  If `method = "k_fold"`, the number of folds to use.

- stack:

  Logical. If `method = "k_fold"`, should performance be computed by
  stacking residuals from each holdout fold and calculating each metric
  on the stacked data (`TRUE`, default) or should performance be
  computed by calculating metrics within each holdout fold and averaging
  performance across each fold (`FALSE`)?

- verbose:

  Toggle warnings.

- ...:

  Not used.

## Value

A data frame with columns for each metric requested, as well as `k` if
`method = "holdout"` and the `Method` used for cross-validation. If
`method = "holdout"` and `stack = TRUE`, the standard error (standard
deviation across holdout folds) for each metric is also included.

## Examples

``` r
model <- lm(mpg ~ wt + cyl, data = mtcars)
performance_cv(model)
#> # Cross-validation performance (30% holdout method)
#> 
#> MSE | RMSE |   R2
#> -----------------
#> 9.2 |    3 | 0.79
```
