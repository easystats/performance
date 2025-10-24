# Reliability Test for Items or Scales

Compute various measures of internal consistencies for tests or
item-scales of questionnaires.

## Usage

``` r
item_reliability(x, standardize = FALSE, digits = 3, verbose = TRUE)
```

## Arguments

- x:

  A matrix or a data frame.

- standardize:

  Logical, if `TRUE`, the data frame's vectors will be standardized.
  Recommended when the variables have different measures / scales.

- digits:

  Amount of digits for returned values.

- verbose:

  Toggle warnings and messages.

## Value

A data frame with the item-total correlations (column
`Item_Total_Correlation`), corrected item-total correlations (*item
discrimination*, column `Discrimination`) and Cronbach's Alpha (if item
deleted, column `Alpha_if_deleted`) for each item of the scale, or
`NULL` if data frame had too less columns.

## Details

This function calculates the item-total correlations, item
discriminations (corrected item-total correlations for each item of `x`
with the remaining items) and the Cronbach's alpha for each item, if it
was deleted from the scale. The absolute value of the item
discrimination indices should be above 0.2. An index between 0.2 and 0.4
is considered as "fair", while an index above 0.4 (or below -0.4) is
"good". The range of satisfactory values is from 0.4 to 0.7. Items with
low discrimination indices are often ambiguously worded and should be
examined. Items with negative indices should be examined to determine
why a negative value was obtained (e.g. reversed answer categories
regarding positive and negative poles).

See
[`check_itemscale()`](https://easystats.github.io/performance/reference/check_itemscale.md)
and
[`item_discrimination()`](https://easystats.github.io/performance/reference/item_discrimination.md)
for more details on the interpretation of the results.

## Note

- *Item difficulty* should range between 0.2 and 0.8. Ideal value is
  `p+(1-p)/2` (which mostly is between 0.5 and 0.8). See
  [`item_difficulty()`](https://easystats.github.io/performance/reference/item_difficulty.md)
  for details.

- For *item discrimination*, also known as *corrected item-total
  correlations*, acceptable values are 0.20 or higher; the closer to
  1.00 the better. See
  [`item_discrimination()`](https://easystats.github.io/performance/reference/item_discrimination.md)
  for more details. If an item discrimination is negative, the
  corresponding item probably need to be reverse-coded (which can be
  done with
  [`datawizard::reverse()`](https://easystats.github.io/datawizard/reference/reverse.html)).

- In case the total *Cronbach's alpha* value is below the acceptable
  cut-off of 0.7 (mostly if an index has few items), the *mean
  inter-item-correlation* is an alternative measure to indicate
  acceptability. Satisfactory range lies between 0.2 and 0.4. See also
  [`item_intercor()`](https://easystats.github.io/performance/reference/item_intercor.md).

## References

- Briggs SR, Cheek JM (1986) The role of factor analysis in the
  development and evaluation of personality scales. Journal of
  Personality, 54(1), 106-148. doi: 10.1111/j.1467-6494.1986.tb00391.x

## Examples

``` r
data(mtcars)
x <- mtcars[, c("cyl", "gear", "carb", "hp")]
item_reliability(x)
#> Some of the values are negative. Maybe affected items need to be
#>   reverse-coded, e.g. using `datawizard::reverse()`.
#> # Item Reliability
#> 
#> Item | Alpha if deleted | Total Correlation | Discrimination
#> ------------------------------------------------------------
#> cyl  |             0.05 |              0.83 |           0.83
#> gear |             0.11 |             -0.12 |          -0.13
#> carb |             0.06 |              0.76 |           0.75
#> hp   |             0.41 |              1.00 |           0.88
#> 
#> Mean inter-item-correlation = 0.294  Cronbach's alpha = 0.095
```
