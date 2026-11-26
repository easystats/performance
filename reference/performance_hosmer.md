# Hosmer-Lemeshow goodness-of-fit test

Check model quality of logistic regression models.

## Usage

``` r
performance_hosmer(model, n_bins = 10, verbose = TRUE)
```

## Arguments

- model:

  A `glm`-object with binomial-family.

- n_bins:

  Numeric, the number of bins to divide the data.

- verbose:

  Toggle messages.

## Value

An object of class `performance_hosmer` with following values: `chisq`,
the Hosmer-Lemeshow chi-squared statistic; `df`, degrees of freedom and
`p.value` the p-value for the goodness-of-fit test. The observed and
expected counts for each group are returned as attributes `observed` and
`expected`, matrices with one row per group and one column each for the
response values 0 and 1.

## Details

A well-fitting model shows *no* significant difference between the model
and the observed data, i.e. the reported p-value should be greater than
0.05.

If fitted values are tied, some cut points between groups are the same.
Each of these is used once, and empty groups are dropped, so there can
be fewer groups than `n_bins`. The degrees of freedom are then based on
the number of groups that were formed.

## References

Hosmer, D. W., and Lemeshow, S. (2000). Applied Logistic Regression.
Hoboken, NJ, USA: John Wiley and Sons, Inc.
[doi:10.1002/0471722146](https://doi.org/10.1002/0471722146)

## Examples

``` r
model <- glm(vs ~ wt + mpg, data = mtcars, family = "binomial")
performance_hosmer(model)
#> # Hosmer-Lemeshow Goodness-of-Fit Test
#> 
#>   Chi-squared: 5.137
#>            df: 8    
#>       p-value: 0.743
#> 
#> Summary: model seems to fit well.
```
