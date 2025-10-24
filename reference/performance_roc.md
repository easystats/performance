# Simple ROC curve

This function calculates a simple ROC curves of x/y coordinates based on
response and predictions of a binomial model.

It returns the area under the curve (AUC) as a percentage, which
corresponds to the probability that a randomly chosen observation of
"condition 1" is correctly classified by the model as having a higher
probability of being "condition 1" than a randomly chosen "condition 2"
observation.

Applying [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html)
to the output returns a data frame containing the following:

- `Sensitivity` (that actually corresponds to `1 - Specificity`): It is
  the False Positive Rate.

- `Sensitivity`: It is the True Positive Rate, which is the proportion
  of correctly classified "condition 1" observations.

## Usage

``` r
performance_roc(x, ..., predictions, new_data)
```

## Arguments

- x:

  A numeric vector, representing the outcome (0/1), or a model with
  binomial outcome.

- ...:

  One or more models with binomial outcome. In this case, `new_data` is
  ignored.

- predictions:

  If `x` is numeric, a numeric vector of same length as `x`,
  representing the actual predicted values.

- new_data:

  If `x` is a model, a data frame that is passed to
  [`predict()`](https://rdrr.io/r/stats/predict.html) as
  `newdata`-argument. If `NULL`, the ROC for the full model is
  calculated.

## Value

A data frame with three columns, the x/y-coordinate pairs for the ROC
curve (`Sensitivity` and `Specificity`), and a column with the model
name.

## Note

There is also a
[[`plot()`](https://rdrr.io/r/graphics/plot.default.html)-method](https://easystats.github.io/see/articles/performance.html)
implemented in the [see-package](https://easystats.github.io/see/).

## Examples

``` r
library(bayestestR)
data(iris)

set.seed(123)
iris$y <- rbinom(nrow(iris), size = 1, .3)
folds <- sample(nrow(iris), size = nrow(iris) / 8, replace = FALSE)
test_data <- iris[folds, ]
train_data <- iris[-folds, ]

model <- glm(y ~ Sepal.Length + Sepal.Width, data = train_data, family = "binomial")
as.data.frame(performance_roc(model, new_data = test_data))
#>    Sensitivity Specificity   Model
#> 1    0.0000000  0.00000000 Model 1
#> 2    0.1428571  0.00000000 Model 1
#> 3    0.1428571  0.09090909 Model 1
#> 4    0.1428571  0.18181818 Model 1
#> 5    0.1428571  0.27272727 Model 1
#> 6    0.1428571  0.36363636 Model 1
#> 7    0.2857143  0.36363636 Model 1
#> 8    0.2857143  0.45454545 Model 1
#> 9    0.2857143  0.54545455 Model 1
#> 10   0.2857143  0.63636364 Model 1
#> 11   0.2857143  0.72727273 Model 1
#> 12   0.4285714  0.72727273 Model 1
#> 13   0.5714286  0.72727273 Model 1
#> 14   0.5714286  0.81818182 Model 1
#> 15   0.7142857  0.81818182 Model 1
#> 16   0.8571429  0.81818182 Model 1
#> 17   0.8571429  0.90909091 Model 1
#> 18   1.0000000  0.90909091 Model 1
#> 19   1.0000000  1.00000000 Model 1
#> 20   1.0000000  1.00000000 Model 1
as.numeric(performance_roc(model))
#> [1] 0.540825

roc <- performance_roc(model, new_data = test_data)
area_under_curve(roc$Specificity, roc$Sensitivity)
#> [1] 0.3766234

if (interactive()) {
  m1 <- glm(y ~ Sepal.Length + Sepal.Width, data = iris, family = "binomial")
  m2 <- glm(y ~ Sepal.Length + Petal.Width, data = iris, family = "binomial")
  m3 <- glm(y ~ Sepal.Length + Species, data = iris, family = "binomial")
  performance_roc(m1, m2, m3)

  # if you have `see` package installed, you can also plot comparison of
  # ROC curves for different models
  if (require("see")) plot(performance_roc(m1, m2, m3))
}
```
