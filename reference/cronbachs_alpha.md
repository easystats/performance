# Cronbach's Alpha for Items or Scales

Compute various measures of internal consistencies for tests or
item-scales of questionnaires. `cronbachs_alpha()` calculates the
Cronbach's Alpha value for all variables in `x`. `item_alpha()` is an
alias for `cronbachs_alpha()`.

## Usage

``` r
cronbachs_alpha(x, ...)

item_alpha(x, ...)

# S3 method for class 'data.frame'
cronbachs_alpha(x, verbose = TRUE, ...)
```

## Arguments

- x:

  A matrix or a data frame, or an object of class `parameters_pca`, as
  returned by
  [`parameters::principal_components()`](https://easystats.github.io/parameters/reference/principal_components.html),
  or an object of class `parameters_efa`, as returned by
  [`parameters::factor_analysis()`](https://easystats.github.io/parameters/reference/principal_components.html).

- ...:

  Currently not used.

- verbose:

  Toggle warnings and messages.

## Value

The Cronbach's Alpha value for `x`.

## Details

The Cronbach's Alpha value for `x`. A value closer to 1 indicates
greater internal consistency, where usually following rule of thumb is
applied to interpret the results:

- α \< 0.5 is unacceptable,

- 0.5 \< α \< 0.6 is poor,

- 0.6 \< α \< 0.7 is questionable,

- 0.7 \< α \< 0.8 is acceptable,

- and everything \> 0.8 is good or excellent.

## Note

`item_alpha()` is an alias for `cronbachs_alpha()`.

## References

Bland, J. M., and Altman, D. G. Statistics notes: Cronbach's alpha. BMJ
1997;314:572. 10.1136/bmj.314.7080.572

## Examples

``` r
data(mtcars)
x <- mtcars[, c("cyl", "gear", "carb", "hp")]
cronbachs_alpha(x)
#> [1] 0.09463206
```
