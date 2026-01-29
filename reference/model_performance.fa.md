# Performance of FA / PCA models

Compute indices of model performance for models from the **psych**
package, and for
[`parameters::factor_analysis()`](https://easystats.github.io/parameters/reference/principal_components.html)
and
[`item_omega()`](https://easystats.github.io/performance/reference/item_omega.md).

## Usage

``` r
# S3 method for class 'fa'
model_performance(model, metrics = "all", verbose = TRUE, ...)
```

## Arguments

- model:

  A model object of class `fa` (e.g., from
  [`psych::fa()`](https://rdrr.io/pkg/psych/man/fa.html)), `principal`
  (e.g., from
  [`psych::principal()`](https://rdrr.io/pkg/psych/man/principal.html)),
  or from
  [`parameters::factor_analysis()`](https://easystats.github.io/parameters/reference/principal_components.html)
  or
  [`item_omega()`](https://easystats.github.io/performance/reference/item_omega.md).

- metrics:

  Can be `"all"` or a character vector of metrics to be computed (some
  of `"Chi2"`, `"Chi2_df"`, `"df"`, `"p_Chi2"`, `"RMSR"`,
  `"RMSR_corrected"`, `"TLI"`, `"RMSEA"`, and `"BIC"`. For omega-models,
  can also include `"R2"` and `"Correlation"`.

- verbose:

  Toggle off warnings.

- ...:

  Arguments passed to or from other methods.

## Value

A data frame (with one row) and one column per "index" (see `metrics`).

## Details

The `RMSR_corrected` metric is the Root Mean Square Residual corrected
for degrees of freedom (i.e., dividing by degrees of freedom rather than
the number of observations).

For omega-models, the columns `R2` and `Correlation` are measures of
factor score adequacy. `R2` refers to the multiple R square of scores
with factors, while `Correlation` indicates the correlation of scores
with factors.

## Examples

``` r
out <- psych::fa(psychTools::bfi[, 1:25], 5)
model_performance(out)
#> # Indices of model performance
#> 
#> Chi2(185) | p (Chi2) |  RMSR | RMSR_corrected |   TLI | RMSEA |   RMSEA 90% CI |   BIC
#> --------------------------------------------------------------------------------------
#> 1808.943  |   < .001 | 0.029 |          0.037 | 0.867 | 0.056 | [0.054, 0.058] | 340.5

out <- item_omega(mtcars, n = 3)
model_performance(out)
#> # Indices of model performance
#> 
#> Model             |    Chi2 | df | p (Chi2) |  RMSR | RMSR_corrected |   TLI
#> ----------------------------------------------------------------------------
#> 3-factor solution |  31.796 | 25 |   0.164  | 0.015 |          0.023 |      
#> g-model           | 264.781 | 44 |   < .001 | 0.393 |          0.440 | 0.195
#> 
#> Model             | RMSEA |   RMSEA 90% CI |   BIC |    R2 | Correlation
#> ------------------------------------------------------------------------
#> 3-factor solution | 0.087 | [0.000, 0.181] | -54.8 |       |            
#> g-model           | 0.395 | [0.356, 0.450] | 112.3 | 0.761 |       0.873
#> 
#> Compare the model fit of the 3-factor solution with the g-only model.
#>   If the g-model has smaller RMSR and RMSEA then your items are more
#>   likely to describe a single unidimensional construct. If the 3-factor
#>   model has smaller RMSR and RMSEA then your construct is more likely to
#>   be made up of 3 sub-constructs.
```
