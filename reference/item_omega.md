# McDonald's Omega for Items or Scales

This function computes McDonald's omega reliability coefficients
alongside Cronbach's alpha for a set of items or a scale. It acts as a
wrapper for the
[`psych::omega()`](https://rdrr.io/pkg/psych/man/omega.html) function.
The aim is to make McDonald's omega readily available and present it
with the widely-known Cronbach's alpha, allowing for a more complete
understanding of scale reliability. The output includes various forms of
omega (e.g., total, hierarchical) depending on the factor structure
specified.

## Usage

``` r
item_omega(x, ...)

# S3 method for class 'data.frame'
item_omega(
  x,
  n = "auto",
  rotation = "oblimin",
  factor_method = "minres",
  poly_cor = FALSE,
  verbose = TRUE,
  ...
)

# S3 method for class 'matrix'
item_omega(
  x,
  n = "auto",
  rotation = "oblimin",
  factor_method = "minres",
  n_obs = NULL,
  poly_cor = FALSE,
  verbose = TRUE,
  ...
)
```

## Arguments

- x:

  A matrix or a data frame.

- ...:

  Additional arguments passed to
  [`psych::omega()`](https://rdrr.io/pkg/psych/man/omega.html).

- n:

  Number of factors to extract.

- rotation:

  Rotation to be applied. Defaults to `"oblimin"`. Further options are
  `"simplimax"`, `"Promax"`, `"cluster"` and `"target"`. See
  [`?psych::omega`](https://rdrr.io/pkg/psych/man/omega.html) for
  details.

- factor_method:

  The factoring method to be used. Passed to the `fm` argument in
  [`psych::omega()`](https://rdrr.io/pkg/psych/man/omega.html). Defaults
  to `"minres"` (minimum residual). Other options include `"ml"`
  (maximum likelihood), `"pa"` (principal axis), etc.

- poly_cor:

  Logical, if `TRUE`, polychoric correlations will be computed (by
  passing `poly = TRUE` to
  [`psych::omega()`](https://rdrr.io/pkg/psych/man/omega.html)).
  Defaults to `FALSE`.

- verbose:

  Logical, if `TRUE` (default), messages are printed.

- n_obs:

  Number of observations in the original data set if `x` is a
  correlation matrix. Required to compute correct fit indices.

## Value

A data frames containing the reliability coefficients. Use
[`summary()`](https://rdrr.io/r/base/summary.html) or
[`parameters::model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.html)
on the returned object to extract more information.

## Details

`item_omega()` is a simple wrapper around
[`psych::omega()`](https://rdrr.io/pkg/psych/man/omega.html), which
returns the reliability coefficients. The original object returned by
[`psych::omega()`](https://rdrr.io/pkg/psych/man/omega.html) is saved as
`$model` attribute. Further information are accessible via the
[`summary()`](https://rdrr.io/r/base/summary.html) and
[`parameters::model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.html)
methods. Use [`as.numeric()`](https://rdrr.io/r/base/numeric.html) to
return the reliability coefficients as (named) numeric vector. Detailed
information can be found in the docs of
[`?psych::omega`](https://rdrr.io/pkg/psych/man/omega.html).

## References

- Bland, J. M., & Altman, D. G. (1997). Statistics notes: Cronbach's
  alpha. BMJ, 314(7080), 572.
  [doi:10.1136/bmj.314.7080.572](https://doi.org/10.1136/bmj.314.7080.572)

- Revelle, W., & Zinbarg, R. E. (2009). Coefficients alpha, beta, omega,
  and the glb: Comments on Sijtsma. Psychometrika, 74(1), 145–154.
  [doi:10.1007/s11336-008-9102-z](https://doi.org/10.1007/s11336-008-9102-z)

- Zinbarg, R.E., Revelle, W., Yovel, I., & Li. W. (2005). Cronbach's
  Alpha, Revelle's Beta, McDonald's Omega: Their relations with each and
  two alternative conceptualizations of reliability. Psychometrika. 70,
  123-133

## Examples

``` r
data(mtcars)
x <- mtcars[1:7]
result <- item_omega(x, n = 2)
#> Loading required namespace: GPArotation
#> 
#> Three factors are required for identification -- general factor loadings set to be equal. 
#> Proceed with caution. 
#> Think about redoing the analysis with alternative values of the 'option' setting.

result
#> # Reliability Coefficients
#> 
#> Statistic            | Coefficient
#> ----------------------------------
#> Alpha                |        0.93
#> G.6                  |        0.97
#> Omega (hierarchical) |        0.46
#> Omega (asymptotic H) |        0.47
#> Omega (total)        |        0.97

as.numeric(result)
#>                Alpha                  G.6 Omega (hierarchical) 
#>            0.9315665            0.9662008            0.4610075 
#> Omega (asymptotic H)        Omega (total) 
#>            0.4745543            0.9714535 

summary(result)
#> # Omega Statistics
#> 
#> Statistic            | Coefficient
#> ----------------------------------
#> Alpha                |        0.93
#> G.6                  |        0.97
#> Omega (hierarchical) |        0.46
#> Omega (asymptotic H) |        0.47
#> Omega (total)        |        0.97
#> 
#> # Omega Coefficients
#> 
#> Composite | Omega (total) | Omega (hierarchical) | Omega (group)
#> ----------------------------------------------------------------
#> g         |          0.97 |                 0.46 |          0.48
#> F1*       |          0.96 |                 0.40 |          0.56
#> F2*       |          1.00 |                 0.37 |          0.63
#> 
#> # Variances
#> 
#> Composite | Total (%) | General Factor (%) | Group Factor (%)
#> -------------------------------------------------------------
#> g         |     97.15 |              46.10 |            48.39
#> F1*       |     95.91 |              39.92 |            55.99
#> F2*       |     99.52 |              36.61 |            62.91

parameters::model_parameters(result)
#> # Rotated loadings from Omega (oblimin-rotation)
#> 
#> Variable |    g |      F1* |   F2* |   h2 |       u2 |   p2 | Complexity
#> ------------------------------------------------------------------------
#> mpg-     | 0.59 |     0.70 |  0.07 | 0.85 |     0.15 | 0.41 |       1.96
#> cyl      | 0.67 |     0.65 |  0.22 | 0.92 |     0.08 | 0.48 |       2.22
#> disp     | 0.61 |     0.73 |  0.07 | 0.91 |     0.09 | 0.41 |       1.96
#> hp       | 0.66 |     0.47 |  0.40 | 0.82 |     0.18 | 0.53 |       2.52
#> drat-    | 0.37 |     0.67 | -0.17 | 0.61 |     0.39 | 0.23 |       1.73
#> wt       | 0.49 |     0.79 | -0.15 | 0.90 |     0.10 | 0.27 |       1.75
#> qsec-    | 0.61 | 2.64e-03 |  0.79 | 1.00 | 4.75e-03 | 0.37 |       1.87
```
