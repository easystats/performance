# Check variables for within- and/or between-group variation

Checks if variables vary within and/or between levels of grouping
variables. This function can be used to infer the hierarchical Design of
a given dataset, or detect any predictors that might cause heterogeneity
bias (*Bell and Jones, 2015*). Use
[`summary()`](https://rdrr.io/r/base/summary.html) on the output if you
are mainly interested if and which predictors are possibly affected by
heterogeneity bias.

## Usage

``` r
check_group_variation(x, ...)

# Default S3 method
check_group_variation(x, ...)

# S3 method for class 'data.frame'
check_group_variation(
  x,
  select = NULL,
  by = NULL,
  include_by = FALSE,
  numeric_as_factor = FALSE,
  tolerance_numeric = 1e-04,
  tolerance_factor = "crossed",
  ...
)

# S3 method for class 'check_group_variation'
summary(object, flatten = FALSE, ...)
```

## Arguments

- x:

  A data frame or a mixed model. See details and examples.

- ...:

  Arguments passed to other methods

- select:

  Character vector (or formula) with names of variables to select that
  should be checked. If `NULL`, selects all variables (except those in
  `by`).

- by:

  Character vector (or formula) with the name of the variable that
  indicates the group- or cluster-ID. For cross-classified or nested
  designs, `by` can also identify two or more variables as group- or
  cluster-IDs.

- include_by:

  When there is more than one grouping variable, should they be check
  against each other?

- numeric_as_factor:

  Should numeric variables be tested as factors?

- tolerance_numeric:

  The minimal percent of variation (observed
  [icc](https://easystats.github.io/performance/reference/icc.md)) that
  is tolerated to indicate no within- or no between-effect.

- tolerance_factor:

  How should a non-numeric variable be identified as varying *only*
  "within" a grouping variable? Options are:

  - `"crossed"` - if all groups have all unique values of X.

  - `"balanced"` - if all groups have all unique values of X, *with
    equal frequency*.

- object:

  result from `check_group_variation()`

- flatten:

  Logical, if `TRUE`, the values are returned as character vector, not
  as list. Duplicated values are removed.

## Value

A data frame with Group, Variable, Variation and Design columns.

## Details

This function attempt to identify the variability of a set of variables
(`select`) with respect to one or more grouping variables (`by`). If `x`
is a (mixed effect) model, the variability of the fixed effects
predictors are checked with respect to the random grouping variables.\
\
Generally, a variable is considered to vary *between* groups if is
correlated with those groups, and to vary *within* groups if it not a
constant within at least one group.

### Numeric variables

Numeric variables are partitioned via
[`datawizard::demean()`](https://easystats.github.io/datawizard/reference/demean.html)
to their within- and between-group components. Then, the variance for
each of these two component is calculated. Variables with within-group
variance larger than `tolerance_numeric` are labeled as *within*,
variables with a between-group variance larger than `tolerance_numeric`
are labeled as *between*, and variables with both variances larger than
`tolerance_numeric` are labeled as *both*.

Setting `numeric_as_factor = TRUE` causes numeric variables to be tested
using the following criteria.

### Non-numeric variables

These variables can have one of the following three labels:

- *between* - the variable is correlated with the groups, *and* is fixed
  within each group (each group has exactly one unique, constant value)

- *within* - the variable is *crossed* with the grouping variable, such
  that all possible values appear within each group. The
  `tolerance_factor` argument controls if full balance is also required.

- *both* - the variable is correlated with the groups, but also varies
  within each group but is not fully crossed (or, when
  `tolerance_factor = "balanced"` the variable is fully crossed, but not
  perfectly balanced).

Additionally, the design of non-numeric variables is also checked to see
if they are *nested* within the groups or is they are *crossed*. This is
indicated by the `Design` column.

### Heterogeneity bias

Variables that vary both within and between groups can cause a
heterogeneity bias (*Bell and Jones, 2015*). It is recommended to center
(person-mean centering) those variables to avoid this bias. See
[`datawizard::demean()`](https://easystats.github.io/datawizard/reference/demean.html)
for further details. Use
[`summary()`](https://rdrr.io/r/base/summary.html) to get a short text
result that indicates if and which predictors are possibly affected by
heterogeneity bias.

## References

- Bell A, Jones K. 2015. Explaining Fixed Effects: Random Effects
  Modeling of Time-Series Cross-Sectional and Panel Data. Political
  Science Research and Methods, 3(1), 133–153.

## See also

For further details, read the vignette
<https://easystats.github.io/parameters/articles/demean.html> and also
see documentation for
[`datawizard::demean()`](https://easystats.github.io/datawizard/reference/demean.html).

## Examples

``` r
data(npk)
check_group_variation(npk, by = "block")
#> Check block variation
#> 
#> Variable | Variation |  Design
#> ------------------------------
#> N        |    within | crossed
#> P        |    within | crossed
#> K        |    within | crossed
#> yield    |      both |        

data(iris)
check_group_variation(iris, by = "Species")
#> Check Species variation
#> 
#> Variable     | Variation | Design
#> ---------------------------------
#> Sepal.Length |      both |       
#> Sepal.Width  |      both |       
#> Petal.Length |      both |       
#> Petal.Width  |      both |       

data(ChickWeight)
check_group_variation(ChickWeight, by = "Chick")
#> Check Chick variation
#> 
#> Variable | Variation | Design
#> -----------------------------
#> weight   |      both |       
#> Time     |      both |       
#> Diet     |   between |       

# A subset of mlmRev::egsingle
egsingle <- data.frame(
  schoolid = factor(rep(c("2020", "2820"), times = c(18, 6))),
  lowinc = rep(c(TRUE, FALSE), times = c(18, 6)),
  childid = factor(rep(
    c("288643371", "292020281", "292020361", "295341521"),
    each = 6
  )),
  female = rep(c(TRUE, FALSE), each = 12),
  year = rep(1:6, times = 4),
  math = c(
    -3.068, -1.13, -0.921, 0.463, 0.021, 2.035,
    -2.732, -2.097, -0.988, 0.227, 0.403, 1.623,
    -2.732, -1.898, -0.921, 0.587, 1.578, 2.3,
    -2.288, -2.162, -1.631, -1.555, -0.725, 0.097
  )
)

result <- check_group_variation(
  egsingle,
  by = c("schoolid", "childid"),
  include_by = TRUE
)
result
#> Check schoolid variation
#> 
#> Variable | Variation | Design
#> -----------------------------
#> childid  |      both | nested
#> lowinc   |   between | nested
#> female   |      both |       
#> year     |    within |       
#> math     |      both |       
#> 
#> Check childid variation
#> 
#> Variable | Variation | Design
#> -----------------------------
#> schoolid |   between |       
#> lowinc   |   between |       
#> female   |   between |       
#> year     |    within |       
#> math     |      both |       

summary(result)
#> Possible heterogeneity bias due to following predictors:
#> - childid:
#>   math
#> - schoolid: childid, female, math


data(sleepstudy, package = "lme4")
check_group_variation(sleepstudy, select = "Days", by = "Subject")
#> Check Subject variation
#> 
#> Variable | Variation | Design
#> -----------------------------
#> Days     |    within |       

# Or
mod <- lme4::lmer(Reaction ~ Days + (Days | Subject), data = sleepstudy)
result <- check_group_variation(mod)
result
#> Check Subject variation
#> 
#> Variable | Variation | Design
#> -----------------------------
#> Days     |    within |       

summary(result)
#> No predictor found that could cause heterogeneity bias.
```
