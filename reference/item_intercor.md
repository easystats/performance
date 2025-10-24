# Mean Inter-Item-Correlation

Compute various measures of internal consistencies for tests or
item-scales of questionnaires.

## Usage

``` r
item_intercor(x, method = "pearson")
```

## Arguments

- x:

  A matrix as returned by the
  [`cor()`](https://rdrr.io/r/stats/cor.html)-function, or a data frame
  with items (e.g. from a test or questionnaire).

- method:

  Correlation computation method. May be one of `"pearson"` (default),
  `"spearman"` or `"kendall"`. You may use initial letter only.

## Value

The mean inter-item-correlation value for `x`.

## Details

This function calculates a mean inter-item-correlation, i.e. a
correlation matrix of `x` will be computed (unless `x` is already a
matrix as returned by the [`cor()`](https://rdrr.io/r/stats/cor.html)
function) and the mean of the sum of all items' correlation values is
returned. Requires either a data frame or a computed
[`cor()`](https://rdrr.io/r/stats/cor.html) object.

"Ideally, the average inter-item correlation for a set of items should
be between 0.20 and 0.40, suggesting that while the items are reasonably
homogeneous, they do contain sufficiently unique variance so as to not
be isomorphic with each other. When values are lower than 0.20, then the
items may not be representative of the same content domain. If values
are higher than 0.40, the items may be only capturing a small bandwidth
of the construct." *(Piedmont 2014)*

## References

Piedmont RL. 2014. Inter-item Correlations. In: Michalos AC (eds)
Encyclopedia of Quality of Life and Well-Being Research. Dordrecht:
Springer, 3303-3304.
[doi:10.1007/978-94-007-0753-5_1493](https://doi.org/10.1007/978-94-007-0753-5_1493)

## Examples

``` r
data(mtcars)
x <- mtcars[, c("cyl", "gear", "carb", "hp")]
item_intercor(x)
#> [1] 0.294155
```
