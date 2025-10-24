# Discrimination and Item-Total Correlation of Questionnaire Items

Compute various measures of internal consistencies for tests or
item-scales of questionnaires. `item_discrimination()` calculates the
corrected item-total correlations for each item of `x` with the
remaining items. `item_totalcor()` by default calculates the item-total
correlations (without correction).

## Usage

``` r
item_discrimination(x, standardize = FALSE, corrected = TRUE, verbose = TRUE)

item_totalcor(x, standardize = FALSE, corrected = FALSE, verbose = TRUE)
```

## Arguments

- x:

  A matrix or a data frame.

- standardize:

  Logical, if `TRUE`, the data frame's vectors will be standardized.
  Recommended when the variables have different measures / scales.

- corrected:

  Logical, if `TRUE`, the item-total correlations are corrected for the
  item itself (default). If `FALSE`, the item-total correlations are
  calculated without correction.

- verbose:

  Toggle warnings and messages.

## Value

A data frame with the item discrimination (*corrected item-total
correlations*) for each item of the scale.

## Details

`item_totalcor()` calculates the item-total correlations (without
correction). A positive item-total correlation indicates that an item
successfully aligns with the overall test, with higher values signifying
a better fit. Conversely, a value near zero suggests the item is not
measuring the intended construct, while a negative correlation is a
major red flag that the item is flawed, miskeyed, or measures the
opposite of what is intended. This means a positive correlation is
desired, a zero correlation is problematic, and a negative correlation
requires immediate attention.

The standard item-total correlation has an inherent flaw: the score of
the item being analyzed is included in the total score. This inclusion
can artificially inflate the correlation coefficient, as an item will
always correlate with itself. The *corrected* item-total correlation, or
*item discrimination*, addresses this issue by calculating the
correlation between the score on a single item and the sum of the scores
of all other items on the scale. This is done with
`item_discrimination()`. The absolute value of the item discrimination
indices should be above `0.2`. An index between `0.2` and `0.4` is
considered as "fair", while a satisfactory index ranges from `0.4` to
`0.7`. Items with low discrimination indices are often ambiguously
worded and should be examined. Items with negative indices should be
examined to determine why a negative value was obtained (e.g. reversed
answer categories regarding positive and negative poles - in such cases,
use
[`datawizard::reverse()`](https://easystats.github.io/datawizard/reference/reverse.html)
to reverse-code items in advance).

**Interpretation of the Corrected Item-Total Correlation Values:**

|  |  |  |
|----|----|----|
| Corrected Item-Total Correlation Value | Interpretation | Action |
| **Above 0.40** | The item has a very good discrimination and is strongly related to the underlying construct. | Retain the item. |
| **0.30 to 0.39** | The item has good discrimination and contributes positively to the scale's internal consistency. | Retain the item. |
| **0.20 to 0.29** | The item has marginal discrimination. While not ideal, it may still be acceptable, especially in shorter scales or when measuring a very broad construct. | Consider revising the item for clarity or content. If other items have stronger correlations, this one might be a candidate for removal if the scale needs to be shortened. |
| **Below 0.20** | The item has poor discrimination. It does not correlate well with the rest of the scale and may be measuring something else. Its inclusion is likely to decrease the overall reliability (e.g., Cronbach's Alpha) of the scale. | Revise the item substantially or, more likely, remove it from the scale. |
| **Negative Value** | The item is negatively related to the rest of the scale. This is a serious issue. | The item must be revised or removed. Check for scoring errors (e.g., a reverse-keyed item that wasn't properly recoded). |

`item_discrimination()` and `item_totalcor()` only differ in the default
value of the `corrected` argument. The former calculates the corrected
item-total correlations, while the latter calculates the item-total
correlations.

## References

- Kelava A, Moosbrugger H (2020). Deskriptivstatistische Itemanalyse und
  Testwertbestimmung. In: Moosbrugger H, Kelava A, editors. Testtheorie
  und Fragebogenkonstruktion. Berlin, Heidelberg: Springer, 143–158

## Examples

``` r
data(mtcars)
x <- mtcars[, c("cyl", "gear", "carb", "hp")]
item_discrimination(x)
#> Some of the values are negative. Maybe affected items need to be
#>   reverse-coded, e.g. using `datawizard::reverse()`.
#> Item Discrimination
#> 
#> Item | Discrimination
#> ---------------------
#> cyl  |           0.83
#> gear |          -0.13
#> carb |           0.75
#> hp   |           0.88
item_totalcor(x)
#> Some of the values are negative. Maybe affected items need to be
#>   reverse-coded, e.g. using `datawizard::reverse()`.
#> Item-Total Correlation
#> 
#> Item | Total Correlation
#> ------------------------
#> cyl  |              0.83
#> gear |             -0.12
#> carb |              0.76
#> hp   |              1.00
```
