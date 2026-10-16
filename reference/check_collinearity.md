# Check for multicollinearity of model terms

`check_collinearity()` checks regression models for multicollinearity by
calculating the (generalized) variance inflation factor (VIF, Fox &
Monette 1992). `multicollinearity()` is an alias for
`check_collinearity()`. `check_concurvity()` is a wrapper around
[`mgcv::concurvity()`](https://rdrr.io/pkg/mgcv/man/concurvity.html),
and can be considered as a collinearity check for smooth terms in GAMs.
Confidence intervals for VIF and tolerance invert the noncentral F
distribution of the auxiliary regression of each term on the other terms
(Kelley 2007).

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

For multinomial models from
[`nnet::multinom()`](https://rdrr.io/pkg/nnet/man/multinom.html),
[`mclogit::mblogit()`](https://melff.github.io/mclogit/reference/mblogit.html),
[`brglm2::brmultinom()`](https://rdrr.io/pkg/brglm2/man/brmultinom.html)
and [`brglm2::bracl()`](https://rdrr.io/pkg/brglm2/man/bracl.html), and
for [`VGAM::vglm()`](https://rdrr.io/pkg/VGAM/man/vglm.html) models with
the multinomial or cumulative family, each term can have its own set of
slopes for each outcome level except one. These models get one VIF per
term, computed from the slopes of that term across all outcome levels,
so the VIF does not depend on the reference level. Parallel models, such
as `bracl(parallel = TRUE)`, have one slope per model matrix column. The
adjusted VIF is then `VIF^(1/(2*<n>))`, where `<n>` is the number of
slopes of the term across all outcome levels.

The confidence interval of a VIF is an interval for the population R2 of
the auxiliary regression of that term on the other terms. The R2 limits
map to the VIF by `1 / (1 - R2)`. The interval inverts the noncentral F
distribution of the F statistic of that regression, as `MBESS::ci.R2()`
does for fixed predictors (Kelley 2007). The interval assumes a linear
model with fixed predictors. For other models, and for terms with more
than one coefficient, it is an approximation. Its degrees of freedom
depend on the number of coefficients of the other terms, so two terms
with the same VIF can get different intervals. For a VIF very close to
1, both limits can be 1.

If the correlation matrix of the coefficients is singular because of
collinearity between model terms, for example for exactly collinear
predictors in a model that keeps all coefficients, such as
[`nnet::multinom()`](https://rdrr.io/pkg/nnet/man/multinom.html),
`check_collinearity()` gives a warning and returns `NULL`.

## Note

There is also a
[`plot()`-method](https://easystats.github.io/see/articles/performance.html)
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

- Kelley, K. (2007). Confidence Intervals for Standardized Effect Sizes:
  Theory, Application, and Implementation. Journal of Statistical
  Software, 20(8), 1–24.
  [doi:10.18637/jss.v020.i08](https://doi.org/10.18637/jss.v020.i08)

- Kim, Y., & Jung, G. (2024). Understanding linear interaction analysis
  with causal graphs. British Journal of Mathematical and Statistical
  Psychology, 00, 1–14.

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
#>  gear 1.53 [1.04,  2.06]     1.24      0.65     [0.48, 0.96]
#> 
#> Moderate Correlation
#> 
#>  Term  VIF    VIF 95% CI adj. VIF Tolerance Tolerance 95% CI
#>    wt 5.05 [2.57,  7.07]     2.25      0.20     [0.14, 0.39]
#>   cyl 5.41 [2.74,  7.58]     2.33      0.18     [0.13, 0.36]
#>  disp 9.97 [4.90, 13.93]     3.16      0.10     [0.07, 0.20]

# plot results
x <- check_collinearity(m)
plot(x)
```
