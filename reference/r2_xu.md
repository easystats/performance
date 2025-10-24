# Xu' R2 (Omega-squared)

Calculates Xu' Omega-squared value, a simple R2 equivalent for linear
mixed models.

## Usage

``` r
r2_xu(model)
```

## Arguments

- model:

  A linear (mixed) model.

## Value

The R2 value.

## Details

`r2_xu()` is a crude measure for the explained variance from linear
(mixed) effects models, which is originally denoted as Ω².

## References

Xu, R. (2003). Measuring explained variation in linear mixed effects
models. Statistics in Medicine, 22(22), 3527–3541.
[doi:10.1002/sim.1572](https://doi.org/10.1002/sim.1572)

## Examples

``` r
model <- lm(Sepal.Length ~ Petal.Length + Species, data = iris)
r2_xu(model)
#>   Xu's R2 
#> 0.8367238 
```
