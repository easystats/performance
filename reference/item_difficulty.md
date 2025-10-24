# Difficulty of Questionnaire Items

Compute various measures of internal consistencies for tests or
item-scales of questionnaires.

## Usage

``` r
item_difficulty(x, maximum_value = NULL)
```

## Arguments

- x:

  Depending on the function, `x` may be a `matrix` as returned by the
  [`cor()`](https://rdrr.io/r/stats/cor.html)-function, or a data frame
  with items (e.g. from a test or questionnaire).

- maximum_value:

  Numeric value, indicating the maximum value of an item. If `NULL`
  (default), the maximum is taken from the maximum value of all columns
  in `x` (assuming that the maximum value at least appears once in the
  data). If `NA`, each item's maximum value is taken as maximum. If the
  required maximum value is not present in the data, specify the
  theoreritcal maximum using `maximum_value`.

## Value

A data frame with three columns: The name(s) of the item(s), the item
difficulties for each item, and the ideal item difficulty.

## Details

*Item difficutly* of an item is defined as the quotient of the sum
actually achieved for this item of all and the maximum achievable score.
This function calculates the item difficulty, which should range between
0.2 and 0.8. Lower values are a signal for more difficult items, while
higher values close to one are a sign for easier items. The ideal value
for item difficulty is `p + (1 - p) / 2`, where `p = 1 / max(x)`. In
most cases, the ideal item difficulty lies between 0.5 and 0.8.

## References

- Bortz, J., and Döring, N. (2006). Quantitative Methoden der
  Datenerhebung. In J. Bortz and N. Döring, Forschungsmethoden und
  Evaluation. Springer: Berlin, Heidelberg: 137–293

- Kelava A, Moosbrugger H (2020). Deskriptivstatistische Itemanalyse und
  Testwertbestimmung. In: Moosbrugger H, Kelava A, editors. Testtheorie
  und Fragebogenkonstruktion. Berlin, Heidelberg: Springer, 143–158

## Examples

``` r
data(mtcars)
x <- mtcars[, c("cyl", "gear", "carb", "hp")]
item_difficulty(x)
#> Item Difficulty
#> 
#> Item | Difficulty | Ideal
#> -------------------------
#> cyl  |       0.02 |  0.50
#> gear |       0.01 |  0.50
#> carb |       0.01 |  0.50
#> hp   |       0.44 |  0.50
```
