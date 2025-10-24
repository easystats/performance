# Check model predictor for heterogeneity bias *(Deprecated)*

`check_heterogeneity_bias()` checks if model predictors or variables may
cause a heterogeneity bias, i.e. if variables have any within-group
variance (*Bell and Jones, 2015*).

**We recommend using
[`check_group_variation()`](https://easystats.github.io/performance/reference/check_group_variation.md)
instead, for a more detailed and flexible examination of group-wise
variability.**

## Usage

``` r
check_heterogeneity_bias(x, select = NULL, by = NULL, nested = FALSE)
```

## Arguments

- x:

  A data frame or a mixed model object.

- select:

  Character vector (or formula) with names of variables to select that
  should be checked. If `x` is a mixed model object, this argument will
  be ignored.

- by:

  Character vector (or formula) with the name of the variable that
  indicates the group- or cluster-ID. For cross-classified or nested
  designs, `by` can also identify two or more variables as group- or
  cluster-IDs. If the data is nested and should be treated as such, set
  `nested = TRUE`. Else, if `by` defines two or more variables and
  `nested = FALSE`, a cross-classified design is assumed. If `x` is a
  model object, this argument will be ignored.

  For nested designs, `by` can be:

  - a character vector with the name of the variable that indicates the
    levels, ordered from *highest* level to *lowest* (e.g.
    `by = c("L4", "L3", "L2")`.

  - a character vector with variable names in the format
    `by = "L4/L3/L2"`, where the levels are separated by `/`.

  See also section *De-meaning for cross-classified designs* and
  *De-meaning for nested designs* in
  [`datawizard::demean()`](https://easystats.github.io/datawizard/reference/demean.html).

- nested:

  Logical, if `TRUE`, the data is treated as nested. If `FALSE`, the
  data is treated as cross-classified. Only applies if `by` contains
  more than one variable.

## References

- Bell A, Jones K. 2015. Explaining Fixed Effects: Random Effects
  Modeling of Time-Series Cross-Sectional and Panel Data. Political
  Science Research and Methods, 3(1), 133–153.

## See also

For further details, read the vignette
<https://easystats.github.io/parameters/articles/demean.html> and also
see documentation for
[`datawizard::demean()`](https://easystats.github.io/datawizard/reference/demean.html).

For a more detailed and flexible examination of group-wise variability,
see
[`check_group_variation()`](https://easystats.github.io/performance/reference/check_group_variation.md).

## Examples

``` r
data(iris)
iris$ID <- sample(1:4, nrow(iris), replace = TRUE) # fake-ID
check_heterogeneity_bias(iris, select = c("Sepal.Length", "Petal.Length"), by = "ID")
#> `check_heterogeneity_bias()` is deprecated. Please use
#>   `check_group_variation()` instead.
#> Possible heterogeneity bias due to following predictors: Sepal.Length, Petal.Length
```
