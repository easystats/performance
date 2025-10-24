# Log Loss

Compute the log loss for models with binary outcome.

## Usage

``` r
performance_logloss(model, verbose = TRUE, ...)
```

## Arguments

- model:

  Model with binary outcome.

- verbose:

  Toggle off warnings.

- ...:

  Currently not used.

## Value

Numeric, the log loss of `model`.

## Details

Logistic regression models predict the probability of an outcome of
being a "success" or "failure" (or 1 and 0 etc.).
`performance_logloss()` evaluates how good or bad the predicted
probabilities are. High values indicate bad predictions, while low
values indicate good predictions. The lower the log-loss, the better the
model predicts the outcome.

## See also

[`performance_score()`](https://easystats.github.io/performance/reference/performance_score.md)

## Examples

``` r
data(mtcars)
m <- glm(formula = vs ~ hp + wt, family = binomial, data = mtcars)
performance_logloss(m)
#> [1] 0.2517054
```
