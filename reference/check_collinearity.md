# Check for multicollinearity of model terms

`check_collinearity()` checks regression models for multicollinearity by
calculating the (generalized) variance inflation factor (VIF, Fox &
Monette 1992). `multicollinearity()` is an alias for
`check_collinearity()`. `check_concurvity()` is a wrapper around
[`mgcv::concurvity()`](https://rdrr.io/pkg/mgcv/man/concurvity.html),
and can be considered as a collinearity check for smooth terms in GAMs.
Confidence intervals for VIF and tolerance are based on Marcoulides et
al. (2019, Appendix B).

## Usage

``` r
check_collinearity(x, ...)

multicollinearity(x, ...)

# Default S3 method
check_collinearity(x, ci = 0.95, verbose = TRUE, ...)

# S3 method for class 'glmmTMB'
check_collinearity(x, component = "all", ci = 0.95, verbose = TRUE, ...)

check_concurvity(x, ...)
```

## Arguments

- x:

  A model object (that should at least respond to
  [`vcov()`](https://rdrr.io/r/stats/vcov.html), and if possible, also
  to [`model.matrix()`](https://rdrr.io/r/stats/model.matrix.html) -
  however, it also should work without
  [`model.matrix()`](https://rdrr.io/r/stats/model.matrix.html)).

- ...:

  Currently not used.

- ci:

  Confidence Interval (CI) level for VIF and tolerance values.

- verbose:

  Toggle off warnings or messages.

- component:

  For models with zero-inflation component, multicollinearity can be
  checked for the conditional model (count component,
  `component = "conditional"` or `component = "count"`), zero-inflation
  component (`component = "zero_inflated"` or `component = "zi"`) or
  both components (`component = "all"`). Following model-classes are
  currently supported: `hurdle`, `zeroinfl`, `zerocount`, `MixMod` and
  `glmmTMB`.

## Value

A data frame with information about name of the model term, the
(generalized) variance inflation factor and associated confidence
intervals, the adjusted VIF, which is the factor by which the standard
error is increased due to possible correlation with other terms
(inflation due to collinearity), and tolerance values (including
confidence intervals), where `tolerance = 1/vif`.

## Details

`check_collinearity()` calculates the generalized variance inflation
factor (Fox & Monette 1992), which also returns valid results for
categorical variables. The *adjusted* VIF is calculated as
`VIF^(1/(2*<nlevels>)` (Fox & Monette 1992), which is identical to the
square root of the VIF for numeric predictors, or for categorical
variables with two levels.

## Note

The code to compute the confidence intervals for the VIF and tolerance
values was adapted from the Appendix B from the Marcoulides et al.
paper. Thus, credits go to these authors the original algorithm. There
is also a
[[`plot()`](https://rdrr.io/r/graphics/plot.default.html)-method](https://easystats.github.io/see/articles/performance.html)
implemented in the [see-package](https://easystats.github.io/see/).

## Multicollinearity

Multicollinearity should not be confused with a raw strong correlation
between predictors. What matters is the association between one or more
predictor variables, *conditional on the other variables in the model*.
In a nutshell, multicollinearity means that once you know the effect of
one predictor, the value of knowing the other predictor is rather low.
Thus, one of the predictors doesn't help much in terms of better
understanding the model or predicting the outcome. As a consequence, if
multicollinearity is a problem, the model seems to suggest that the
predictors in question don't seems to be reliably associated with the
outcome (low estimates, high standard errors), although these predictors
actually are strongly associated with the outcome, i.e. indeed might
have strong effect (*McElreath 2020, chapter 6.1*).

Multicollinearity might arise when a third, unobserved variable has a
causal effect on each of the two predictors that are associated with the
outcome. In such cases, the actual relationship that matters would be
the association between the unobserved variable and the outcome.

Remember: "Pairwise correlations are not the problem. It is the
conditional associations - not correlations - that matter." (*McElreath
2020, p. 169*)

## Interpretation of the Variance Inflation Factor

The variance inflation factor is a measure to analyze the magnitude of
multicollinearity of model terms. A VIF less than 5 indicates a low
correlation of that predictor with other predictors. A value between 5
and 10 indicates a moderate correlation, while VIF values larger than 10
are a sign for high, not tolerable correlation of model predictors
(*James et al. 2013*). The *adjusted VIF* column in the output indicates
how much larger the standard error is due to the association with other
predictors conditional on the remaining variables in the model. Note
that these thresholds, although commonly used, are also criticized for
being too high. *Zuur et al. (2010)* suggest using lower values, e.g. a
VIF of 3 or larger may already no longer be considered as "low".

## Multicollinearity and Interaction Terms

If interaction terms are included in a model, high VIF values are
expected. This portion of multicollinearity among the component terms of
an interaction is also called "inessential ill-conditioning", which
leads to inflated VIF values that are typically seen for models with
interaction terms *(Francoeur 2013)*. Centering interaction terms can
resolve this issue *(Kim and Jung 2024)*.

## Multicollinearity and Polynomial Terms

Polynomial transformations are considered a single term and thus VIFs
are not calculated between them.

## Concurvity for Smooth Terms in Generalized Additive Models

`check_concurvity()` is a wrapper around
[`mgcv::concurvity()`](https://rdrr.io/pkg/mgcv/man/concurvity.html),
and can be considered as a collinearity check for smooth terms in
GAMs."Concurvity occurs when some smooth term in a model could be
approximated by one or more of the other smooth terms in the model."
(see
[`?mgcv::concurvity`](https://rdrr.io/pkg/mgcv/man/concurvity.html)).
`check_concurvity()` returns a column named *VIF*, which is the "worst"
measure. While
[`mgcv::concurvity()`](https://rdrr.io/pkg/mgcv/man/concurvity.html)
range between 0 and 1, the *VIF* value is `1 / (1 - worst)`, to make
interpretation comparable to classical VIF values, i.e. `1` indicates no
problems, while higher values indicate increasing lack of
identifiability. The *VIF proportion* column equals the "estimate"
column from
[`mgcv::concurvity()`](https://rdrr.io/pkg/mgcv/man/concurvity.html),
ranging from 0 (no problem) to 1 (total lack of identifiability).

## References

- Fox, J., & Monette, G. (1992). Generalized Collinearity Diagnostics.
  Journal of the American Statistical Association, 87(417), 178–183.

- Francoeur, R. B. (2013). Could Sequential Residual Centering Resolve
  Low Sensitivity in Moderated Regression? Simulations and Cancer
  Symptom Clusters. Open Journal of Statistics, 03(06), 24-44.

- James, G., Witten, D., Hastie, T., and Tibshirani, R. (eds.). (2013).
  An introduction to statistical learning: with applications in R. New
  York: Springer.

- Kim, Y., & Jung, G. (2024). Understanding linear interaction analysis
  with causal graphs. British Journal of Mathematical and Statistical
  Psychology, 00, 1–14.

- Marcoulides, K. M., and Raykov, T. (2019). Evaluation of Variance
  Inflation Factors in Regression Models Using Latent Variable Modeling
  Methods. Educational and Psychological Measurement, 79(5), 874–882.

- McElreath, R. (2020). Statistical rethinking: A Bayesian course with
  examples in R and Stan. 2nd edition. Chapman and Hall/CRC.

- Vanhove, J. (2021) Collinearity Isn’t a Disease That Needs Curing.
  Meta-Psychology, 5.
  [doi:10.15626/MP.2021.2548](https://doi.org/10.15626/MP.2021.2548)

- Zuur AF, Ieno EN, Elphick CS. A protocol for data exploration to avoid
  common statistical problems: Data exploration. Methods in Ecology and
  Evolution (2010) 1:3–14.

## See also

[`see::plot.see_check_collinearity()`](https://easystats.github.io/see/reference/plot.see_check_collinearity.html)
for options to customize the plot.

Other functions to check model assumptions and and assess model quality:
[`check_autocorrelation()`](https://easystats.github.io/performance/reference/check_autocorrelation.md),
[`check_convergence()`](https://easystats.github.io/performance/reference/check_convergence.md),
[`check_heteroscedasticity()`](https://easystats.github.io/performance/reference/check_heteroscedasticity.md),
[`check_homogeneity()`](https://easystats.github.io/performance/reference/check_homogeneity.md),
[`check_model()`](https://easystats.github.io/performance/reference/check_model.md),
[`check_outliers()`](https://easystats.github.io/performance/reference/check_outliers.md),
[`check_overdispersion()`](https://easystats.github.io/performance/reference/check_overdispersion.md),
[`check_predictions()`](https://easystats.github.io/performance/reference/check_predictions.md),
[`check_singularity()`](https://easystats.github.io/performance/reference/check_singularity.md),
[`check_zeroinflation()`](https://easystats.github.io/performance/reference/check_zeroinflation.md)

## Examples

``` r
m <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
check_collinearity(m)
#> # Check for Multicollinearity
#> 
#> Low Correlation
#> 
#>  Term  VIF    VIF 95% CI adj. VIF Tolerance Tolerance 95% CI
#>  gear 1.53 [1.19,  2.51]     1.24      0.65     [0.40, 0.84]
#> 
#> Moderate Correlation
#> 
#>  Term  VIF    VIF 95% CI adj. VIF Tolerance Tolerance 95% CI
#>    wt 5.05 [3.21,  8.41]     2.25      0.20     [0.12, 0.31]
#>   cyl 5.41 [3.42,  9.04]     2.33      0.18     [0.11, 0.29]
#>  disp 9.97 [6.08, 16.85]     3.16      0.10     [0.06, 0.16]

# plot results
x <- check_collinearity(m)
plot(x)
```
