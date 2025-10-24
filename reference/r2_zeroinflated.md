# R2 for models with zero-inflation

Calculates R2 for models with zero-inflation component, including mixed
effects models.

## Usage

``` r
r2_zeroinflated(model, method = "default")
```

## Arguments

- model:

  A model.

- method:

  Indicates the method to calculate R2. Can be `"default"` or
  `"correlation"`. See 'Details'. May be abbreviated.

## Value

For the default-method, a list with the R2 and adjusted R2 values. For
`method = "correlation"`, a named numeric vector with the
correlation-based R2 value.

## Details

The default-method calculates an R2 value based on the residual variance
divided by the total variance. For `method = "correlation"`, R2 is a
correlation-based measure, which is rather crude. It simply computes the
squared correlation between the model's actual and predicted response.

## Examples

``` r
# \donttest{
if (require("pscl")) {
  data(bioChemists)
  model <- zeroinfl(
    art ~ fem + mar + kid5 + ment | kid5 + phd,
    data = bioChemists
  )

  r2_zeroinflated(model)
}
#> Loading required package: pscl
#> Classes and Methods for R originally developed in the
#> Political Science Computational Laboratory
#> Department of Political Science
#> Stanford University (2002-2015),
#> by and under the direction of Simon Jackman.
#> hurdle and zeroinfl functions by Achim Zeileis.
#> # R2 for Zero-Inflated and Hurdle Regression
#>        R2: 0.180
#>   adj. R2: 0.175
# }
```
