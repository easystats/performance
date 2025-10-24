# Somers' Dxy rank correlation for binary outcomes

Calculates the Somers' Dxy rank correlation for logistic regression
models.

## Usage

``` r
r2_somers(model)
```

## Arguments

- model:

  A logistic regression model.

## Value

A named vector with the R2 value.

## References

Somers, R. H. (1962). A new asymmetric measure of association for
ordinal variables. American Sociological Review. 27 (6).

## Examples

``` r
# \donttest{
if (require("correlation") && require("Hmisc")) {
  model <- glm(vs ~ wt + mpg, data = mtcars, family = "binomial")
  r2_somers(model)
}
#> Loading required package: correlation
#> Loading required package: Hmisc
#> 
#> Attaching package: ‘Hmisc’
#> The following object is masked from ‘package:psych’:
#> 
#>     describe
#> The following objects are masked from ‘package:base’:
#> 
#>     format.pval, units
#> Somers' Dxy 
#>   0.8253968 
# }
```
