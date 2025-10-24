# Print tables in different output formats

Prints tables (i.e. data frame) in different output formats.

## Usage

``` r
# S3 method for class 'performance_model'
display(object, format = "markdown", digits = 2, caption = NULL, ...)

# S3 method for class 'performance_model'
print(x, digits = 3, layout = "horizontal", ...)

# S3 method for class 'performance_model'
print_md(
  x,
  digits = 2,
  caption = "Indices of model performance",
  layout = "horizontal",
  ...
)

# S3 method for class 'compare_performance'
print_md(
  x,
  digits = 2,
  caption = "Comparison of Model Performance Indices",
  layout = "horizontal",
  ...
)
```

## Arguments

- object, x:

  An object returned by one of the package's function, for example
  [`model_performance()`](https://easystats.github.io/performance/reference/model_performance.md),
  [`compare_performance()`](https://easystats.github.io/performance/reference/compare_performance.md),
  or
  [`check_itemscale()`](https://easystats.github.io/performance/reference/check_itemscale.md).

- format:

  String, indicating the output format. Can be `"markdown"` `"html"`, or
  `"tt"`. `format = "tt"` creates a `tinytable` object, which is either
  printed as markdown or HTML table, depending on the environment. See
  [`insight::export_table()`](https://easystats.github.io/insight/reference/export_table.html)
  for details.

- digits:

  Number of decimal places.

- caption:

  Table caption as string. If `NULL`, no table caption is printed.

- ...:

  Arguments passed to other methods, e.g.
  [`format()`](https://rdrr.io/r/base/format.html) (and thereby to
  [`insight::format_table()`](https://easystats.github.io/insight/reference/format_table.html)
  or
  [`insight::export_table()`](https://easystats.github.io/insight/reference/export_table.html).
  See related documentation for details on available arguments. For
  example, to control digits for information criteria like AIC or BIC,
  use `ic_digits = <value>`.

- layout:

  Table layout (can be either `"horizontal"` or `"vertical"`).

## Value

A character vector. If `format = "markdown"`, the return value will be a
character vector in markdown-table format.

## Details

[`display()`](https://easystats.github.io/insight/reference/display.html)
is useful when the table-output from functions, which is usually printed
as formatted text-table to console, should be formatted for pretty
table-rendering in markdown documents, or if knitted from rmarkdown to
PDF or Word files. See
[vignette](https://easystats.github.io/parameters/articles/model_parameters_formatting.html)
for examples.

## Global Options to Customize Output when Printing

- `easystats_display_format`:
  `options(easystats_display_format = <value>)` will set the default
  format for the
  [`display()`](https://easystats.github.io/insight/reference/display.html)
  methods. Can be one of `"markdown"`, `"html"`, or `"tt"`.

## Examples

``` r
model <- lm(mpg ~ wt + cyl, data = mtcars)
mp <- model_performance(model)
display(mp)
#> 
#> 
#> |AIC   |  AICc |   BIC |   R2 | R2 (adj.) | RMSE | Sigma |
#> |:-----|:-----:|:-----:|:----:|:---------:|:----:|:-----:|
#> |156.0 | 157.5 | 161.9 | 0.83 |      0.82 | 2.44 |  2.57 |
display(mp, digits = 3, ic_digits = 4)
#> 
#> 
#> |AIC      |     AICc |      BIC |    R2 | R2 (adj.) |  RMSE | Sigma |
#> |:--------|:--------:|:--------:|:-----:|:---------:|:-----:|:-----:|
#> |156.0101 | 157.4915 | 161.8730 | 0.830 |     0.819 | 2.444 | 2.568 |
```
