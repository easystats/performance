# Percentage of Correct Predictions

Percentage of correct predictions (PCP) for models with binary outcome.

## Usage

``` r
performance_pcp(model, ci = 0.95, method = "Herron", verbose = TRUE)
```

## Arguments

- model:

  Model with binary outcome.

- ci:

  The level of the confidence interval.

- method:

  Name of the method to calculate the PCP (see 'Details'). Default is
  `"Herron"`. May be abbreviated.

- verbose:

  Toggle off warnings.

## Value

A list with several elements: the percentage of correct predictions of
the full and the null model, their confidence intervals, as well as the
chi-squared and p-value from the Likelihood-Ratio-Test between the full
and null model.

## Details

`method = "Gelman-Hill"` (or `"gelman_hill"`) computes the PCP based on
the proposal from *Gelman and Hill 2017, 99*, which is defined as the
proportion of cases for which the deterministic prediction is wrong,
i.e. the proportion where the predicted probability is above 0.5,
although y=0 (and vice versa) (see also *Herron 1999, 90*).

`method = "Herron"` (or `"herron"`) computes a modified version of the
PCP (*Herron 1999, 90-92*), which is the sum of predicted probabilities,
where y=1, plus the sum of 1 - predicted probabilities, where y=0,
divided by the number of observations. This approach is said to be more
accurate.

The PCP ranges from 0 to 1, where values closer to 1 mean that the model
predicts the outcome better than models with an PCP closer to 0. In
general, the PCP should be above 0.5 (i.e. 50\\ Furthermore, the PCP of
the full model should be considerably above the null model's PCP.

The likelihood-ratio test indicates whether the model has a
significantly better fit than the null-model (in such cases, p \< 0.05).

## References

- Herron, M. (1999). Postestimation Uncertainty in Limited Dependent
  Variable Models. Political Analysis, 8, 83–98.

- Gelman, A., and Hill, J. (2007). Data analysis using regression and
  multilevel/hierarchical models. Cambridge; New York: Cambridge
  University Press, 99.

## Examples

``` r
data(mtcars)
m <- glm(formula = vs ~ hp + wt, family = binomial, data = mtcars)
performance_pcp(m)
#> # Percentage of Correct Predictions from Logistic Regression Model
#> 
#>   Full model: 83.75% [70.96% - 96.53%]
#>   Null model: 50.78% [33.46% - 68.10%]
#> 
#> # Likelihood-Ratio-Test
#> 
#>   Chi-squared: 27.751
#>   df:  2.000
#>   p-value:  0.000
#> 
performance_pcp(m, method = "Gelman-Hill")
#> # Percentage of Correct Predictions from Logistic Regression Model
#> 
#>   Full model: 87.50% [76.04% - 98.96%]
#>   Null model: 56.25% [39.06% - 73.44%]
#> 
#> # Likelihood-Ratio-Test
#> 
#>   Chi-squared: 27.751
#>   df:  2.000
#>   p-value:  0.000
#> 
```
