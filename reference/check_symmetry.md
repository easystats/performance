# Check distribution symmetry

Uses Hotelling and Solomons test of symmetry by testing if the
standardized nonparametric skew (\\\frac{(Mean - Median)}{SD}\\) is
different than 0.\
\
This is an underlying assumption of Wilcoxon signed-rank test.

## Usage

``` r
check_symmetry(x, ...)
```

## Arguments

- x:

  Model or numeric vector

- ...:

  Not used.

## Examples

``` r
V <- suppressWarnings(wilcox.test(mtcars$mpg))
check_symmetry(V)
#> OK: Data appears symmetrical (p = 0.119).
#> 
```
