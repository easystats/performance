# Efron's R2

Calculates Efron's pseudo R2.

## Usage

``` r
r2_efron(model)
```

## Arguments

- model:

  Generalized linear model.

## Value

The R2 value.

## Details

Efron's R2 is calculated by taking the sum of the squared model
residuals, divided by the total variability in the dependent variable.
This R2 equals the squared correlation between the predicted values and
actual values, however, note that model residuals from generalized
linear models are not generally comparable to those of OLS.

## References

Efron, B. (1978). Regression and ANOVA with zero-one data: Measures of
residual variation. Journal of the American Statistical Association, 73,
113-121.

## Examples

``` r
## Dobson (1990) Page 93: Randomized Controlled Trial:
counts <- c(18, 17, 15, 20, 10, 20, 25, 13, 12) #
outcome <- gl(3, 1, 9)
treatment <- gl(3, 3)
model <- glm(counts ~ outcome + treatment, family = poisson())

r2_efron(model)
#> [1] 0.5265152
```
