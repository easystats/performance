# Accuracy of predictions from model fit

This function calculates the predictive accuracy of linear or logistic
regression models.

## Usage

``` r
performance_accuracy(
  model,
  method = "cv",
  k = 5,
  n = 1000,
  ci = 0.95,
  verbose = TRUE
)
```

## Arguments

- model:

  A linear or logistic regression model. A mixed-effects model is also
  accepted.

- method:

  Character string, indicating whether cross-validation
  (`method = "cv"`) or bootstrapping (`method = "boot"`) is used to
  compute the accuracy values.

- k:

  The number of folds for the k-fold cross-validation.

- n:

  Number of bootstrap-samples.

- ci:

  The level of the confidence interval.

- verbose:

  Toggle warnings.

## Value

A list with three values: The `Accuracy` of the model predictions, i.e.
the proportion of accurately predicted values from the model, its
standard error, `SE`, and the `Method` used to compute the accuracy.

## Details

For linear models, the accuracy is the correlation coefficient between
the actual and the predicted value of the outcome. For logistic
regression models, the accuracy corresponds to the AUC-value, calculated
with the
[`bayestestR::auc()`](https://easystats.github.io/bayestestR/reference/area_under_curve.html)-function.

The accuracy is the mean value of multiple correlation resp. AUC-values,
which are either computed with cross-validation or non-parametric
bootstrapping (see argument `method`). The standard error is the
standard deviation of the computed correlation resp. AUC-values.

## Examples

``` r
model <- lm(mpg ~ wt + cyl, data = mtcars)
performance_accuracy(model)
#> # Accuracy of Model Predictions
#> 
#> Accuracy (95% CI): 92.04% [87.66%, 98.19%]
#> Method: Correlation between observed and predicted

model <- glm(vs ~ wt + mpg, data = mtcars, family = "binomial")
performance_accuracy(model)
#> # Accuracy of Model Predictions
#> 
#> Accuracy (95% CI): 90.00% [75.00%, 100.00%]
#> Method: Area under Curve
```
