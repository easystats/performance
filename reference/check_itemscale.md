# Describe Properties of Item Scales

Compute various measures of internal consistencies applied to
(sub)scales, which items were extracted using
[`parameters::principal_components()`](https://easystats.github.io/parameters/reference/principal_components.html)
or
[`parameters::factor_analysis()`](https://easystats.github.io/parameters/reference/principal_components.html).

## Usage

``` r
check_itemscale(x, factor_index = NULL, verbose = TRUE)
```

## Arguments

- x:

  An object of class `parameters_pca`, as returned by
  [`parameters::principal_components()`](https://easystats.github.io/parameters/reference/principal_components.html),
  of class `parameters_efa`, as returned by
  [`parameters::factor_analysis()`](https://easystats.github.io/parameters/reference/principal_components.html),
  or a data frame.

- factor_index:

  If `x` is a data frame, `factor_index` must be specified. It must be a
  numeric vector of same length as number of columns in `x`, where each
  element is the index of the factor to which the respective column in
  `x`.

- verbose:

  Toggle warnings and messages. If `TRUE`, messages are printed.

## Value

A list of data frames, with related measures of internal consistencies
of each subscale.

## Details

`check_itemscale()` calculates various measures of internal
consistencies, such as Cronbach's alpha, item difficulty or
discrimination etc. on subscales which were built from several items.
Subscales are retrieved from the results of
[`parameters::principal_components()`](https://easystats.github.io/parameters/reference/principal_components.html)
or
[`parameters::factor_analysis()`](https://easystats.github.io/parameters/reference/principal_components.html),
i.e. based on how many components were extracted from the PCA,
respectively how many factors were extracted from the factor analysis.
`check_itemscale()` retrieves those variables that belong to a component
and calculates the above mentioned measures.

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
# data generation from '?prcomp', slightly modified
C <- chol(S <- toeplitz(0.9^(0:15)))
set.seed(17)
X <- matrix(rnorm(1600), 100, 16)
Z <- X %*% C

pca <- parameters::principal_components(
  as.data.frame(Z),
  rotation = "varimax",
  n = 3
)
pca
#> # Rotated loadings from Principal Component Analysis (varimax-rotation)
#> 
#> Variable |  RC3 |  RC1 |  RC2 | Complexity | Uniqueness |  MSA
#> --------------------------------------------------------------
#> V1       | 0.85 | 0.17 | 0.20 |       1.20 |       0.21 | 0.90
#> V2       | 0.89 | 0.25 | 0.22 |       1.28 |       0.11 | 0.90
#> V3       | 0.91 | 0.26 | 0.17 |       1.23 |       0.07 | 0.89
#> V4       | 0.88 | 0.33 | 0.13 |       1.33 |       0.10 | 0.91
#> V5       | 0.82 | 0.41 | 0.14 |       1.55 |       0.14 | 0.94
#> V6       | 0.68 | 0.59 | 0.18 |       2.12 |       0.15 | 0.92
#> V7       | 0.57 | 0.74 | 0.20 |       2.04 |       0.09 | 0.93
#> V8       | 0.44 | 0.81 | 0.20 |       1.67 |       0.11 | 0.95
#> V9       | 0.33 | 0.84 | 0.32 |       1.61 |       0.09 | 0.93
#> V10      | 0.29 | 0.85 | 0.33 |       1.55 |       0.09 | 0.92
#> V11      | 0.30 | 0.79 | 0.42 |       1.86 |       0.11 | 0.92
#> V12      | 0.27 | 0.68 | 0.57 |       2.28 |       0.15 | 0.90
#> V13      | 0.20 | 0.55 | 0.71 |       2.06 |       0.15 | 0.90
#> V14      | 0.21 | 0.36 | 0.86 |       1.48 |       0.09 | 0.91
#> V15      | 0.20 | 0.23 | 0.91 |       1.23 |       0.08 | 0.88
#> V16      | 0.11 | 0.15 | 0.90 |       1.09 |       0.15 | 0.87
#> 
#> The 3 principal components (varimax rotation) accounted for 88.19% of the total variance of the original data (RC3 = 32.81%, RC1 = 31.24%, RC2 = 24.14%).
#> 
check_itemscale(pca)
#> # Description of (Sub-)Scales
#> Component 1
#> 
#> Item | Missings |  Mean |   SD | Skewness | Difficulty | Discrimination | alpha if deleted
#> ------------------------------------------------------------------------------------------
#> V1   |        0 | -0.02 | 1.06 |    -0.49 |      -0.01 |           0.80 |             0.96
#> V2   |        0 | -0.05 | 1.05 |    -0.29 |      -0.02 |           0.90 |             0.95
#> V3   |        0 |  0.00 | 1.10 |    -0.77 |       0.00 |           0.94 |             0.95
#> V4   |        0 |  0.00 | 1.10 |    -0.82 |       0.00 |           0.92 |             0.95
#> V5   |        0 | -0.07 | 1.09 |    -0.29 |      -0.02 |           0.90 |             0.95
#> V6   |        0 | -0.04 | 1.13 |    -0.27 |      -0.01 |           0.83 |             0.96
#> 
#> Mean inter-item-correlation = 0.813  Cronbach's alpha = 0.963
#> 
#> Component 2
#> 
#> Item | Missings |  Mean |   SD | Skewness | Difficulty | Discrimination | alpha if deleted
#> ------------------------------------------------------------------------------------------
#> V7   |        0 | -0.01 | 1.07 |     0.01 |       0.00 |           0.87 |             0.97
#> V8   |        0 |  0.02 | 0.96 |     0.23 |       0.01 |           0.89 |             0.96
#> V9   |        0 |  0.04 | 0.98 |     0.37 |       0.01 |           0.93 |             0.96
#> V10  |        0 |  0.08 | 1.00 |     0.18 |       0.02 |           0.93 |             0.96
#> V11  |        0 |  0.02 | 1.03 |     0.18 |       0.01 |           0.92 |             0.96
#> V12  |        0 |  0.00 | 1.04 |     0.27 |       0.00 |           0.84 |             0.97
#> 
#> Mean inter-item-correlation = 0.840  Cronbach's alpha = 0.969
#> 
#> Component 3
#> 
#> Item | Missings |  Mean |   SD | Skewness | Difficulty | Discrimination | alpha if deleted
#> ------------------------------------------------------------------------------------------
#> V13  |        0 |  0.04 | 0.95 |     0.10 |       0.01 |           0.81 |             0.95
#> V14  |        0 | -0.02 | 0.96 |     0.24 |      -0.01 |           0.93 |             0.91
#> V15  |        0 | -0.03 | 0.94 |     0.41 |      -0.01 |           0.92 |             0.91
#> V16  |        0 |  0.03 | 0.96 |     0.28 |       0.01 |           0.82 |             0.94
#> 
#> Mean inter-item-correlation = 0.811  Cronbach's alpha = 0.945

# as data frame
check_itemscale(
  as.data.frame(Z),
  factor_index = parameters::closest_component(pca)
)
#> # Description of (Sub-)Scales
#> Component 1
#> 
#> Item | Missings |  Mean |   SD | Skewness | Difficulty | Discrimination | alpha if deleted
#> ------------------------------------------------------------------------------------------
#> V1   |        0 | -0.02 | 1.06 |    -0.49 |      -0.01 |           0.80 |             0.96
#> V2   |        0 | -0.05 | 1.05 |    -0.29 |      -0.02 |           0.90 |             0.95
#> V3   |        0 |  0.00 | 1.10 |    -0.77 |       0.00 |           0.94 |             0.95
#> V4   |        0 |  0.00 | 1.10 |    -0.82 |       0.00 |           0.92 |             0.95
#> V5   |        0 | -0.07 | 1.09 |    -0.29 |      -0.02 |           0.90 |             0.95
#> V6   |        0 | -0.04 | 1.13 |    -0.27 |      -0.01 |           0.83 |             0.96
#> 
#> Mean inter-item-correlation = 0.813  Cronbach's alpha = 0.963
#> 
#> Component 2
#> 
#> Item | Missings |  Mean |   SD | Skewness | Difficulty | Discrimination | alpha if deleted
#> ------------------------------------------------------------------------------------------
#> V7   |        0 | -0.01 | 1.07 |     0.01 |       0.00 |           0.87 |             0.97
#> V8   |        0 |  0.02 | 0.96 |     0.23 |       0.01 |           0.89 |             0.96
#> V9   |        0 |  0.04 | 0.98 |     0.37 |       0.01 |           0.93 |             0.96
#> V10  |        0 |  0.08 | 1.00 |     0.18 |       0.02 |           0.93 |             0.96
#> V11  |        0 |  0.02 | 1.03 |     0.18 |       0.01 |           0.92 |             0.96
#> V12  |        0 |  0.00 | 1.04 |     0.27 |       0.00 |           0.84 |             0.97
#> 
#> Mean inter-item-correlation = 0.840  Cronbach's alpha = 0.969
#> 
#> Component 3
#> 
#> Item | Missings |  Mean |   SD | Skewness | Difficulty | Discrimination | alpha if deleted
#> ------------------------------------------------------------------------------------------
#> V13  |        0 |  0.04 | 0.95 |     0.10 |       0.01 |           0.81 |             0.95
#> V14  |        0 | -0.02 | 0.96 |     0.24 |      -0.01 |           0.93 |             0.91
#> V15  |        0 | -0.03 | 0.94 |     0.41 |      -0.01 |           0.92 |             0.91
#> V16  |        0 |  0.03 | 0.96 |     0.28 |       0.01 |           0.82 |             0.94
#> 
#> Mean inter-item-correlation = 0.811  Cronbach's alpha = 0.945
```
