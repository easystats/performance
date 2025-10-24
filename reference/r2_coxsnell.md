# Cox & Snell's R2

Calculates the pseudo-R2 value based on the proposal from *Cox & Snell
(1989)*.

## Usage

``` r
r2_coxsnell(model, ...)
```

## Arguments

- model:

  Model with binary outcome.

- ...:

  Currently not used.

## Value

A named vector with the R2 value.

## Details

This index was proposed by *Cox and Snell (1989, pp. 208-9)* and,
apparently independently, by *Magee (1990)*; but had been suggested
earlier for binary response models by *Maddala (1983)*. However, this
index achieves a maximum of less than 1 for discrete models (i.e. models
whose likelihood is a product of probabilities) which have a maximum of
1, instead of densities, which can become infinite *(Nagelkerke, 1991)*.

## References

- Cox, D. R., Snell, E. J. (1989). Analysis of binary data (Vol. 32).
  Monographs on Statistics and Applied Probability.

- Magee, L. (1990). R 2 measures based on Wald and likelihood ratio
  joint significance tests. The American Statistician, 44(3), 250-253.

- Maddala, G. S. (1986). Limited-dependent and qualitative variables in
  econometrics (No. 3). Cambridge university press.

- Nagelkerke, N. J. (1991). A note on a general definition of the
  coefficient of determination. Biometrika, 78(3), 691-692.

## Examples

``` r
model <- glm(vs ~ wt + mpg, data = mtcars, family = "binomial")
r2_coxsnell(model)
#> Cox & Snell's R2 
#>        0.4401407 
```
