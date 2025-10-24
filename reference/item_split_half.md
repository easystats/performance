# Split-Half Reliability

Compute various measures of internal consistencies for tests or
item-scales of questionnaires.

## Usage

``` r
item_split_half(x, digits = 3)
```

## Arguments

- x:

  A matrix or a data frame.

- digits:

  Amount of digits for returned values.

## Value

A list with two elements: the split-half reliability `splithalf` and the
Spearman-Brown corrected split-half reliability `spearmanbrown`.

## Details

This function calculates the split-half reliability for items in `x`,
including the Spearman-Brown adjustment. Splitting is done by selecting
odd versus even columns in `x`. A value closer to 1 indicates greater
internal consistency.

## References

- Spearman C. 1910. Correlation calculated from faulty data. British
  Journal of Psychology (3): 271-295.
  [doi:10.1111/j.2044-8295.1910.tb00206.x](https://doi.org/10.1111/j.2044-8295.1910.tb00206.x)

- Brown W. 1910. Some experimental results in the correlation of mental
  abilities. British Journal of Psychology (3): 296-322.
  [doi:10.1111/j.2044-8295.1910.tb00207.x](https://doi.org/10.1111/j.2044-8295.1910.tb00207.x)

## Examples

``` r
data(mtcars)
x <- mtcars[, c("cyl", "gear", "carb", "hp")]
item_split_half(x)
#> $splithalf
#> [1] 0.9070215
#> 
#> $spearmanbrown
#> [1] 0.9512441
#> 
```
