# McKelvey & Zavoinas R2

Calculates McKelvey and Zavoinas pseudo R2.

## Usage

``` r
r2_mckelvey(model)
```

## Arguments

- model:

  Generalized linear model.

## Value

The R2 value.

## Details

McKelvey and Zavoinas R2 is based on the explained variance, where the
variance of the predicted response is divided by the sum of the variance
of the predicted response and residual variance. For binomial models,
the residual variance is either `pi^2/3` for logit-link and 1 for
probit-link. For poisson-models, the residual variance is based on
log-normal approximation, similar to the *distribution-specific
variance* as described in
[`?insight::get_variance`](https://easystats.github.io/insight/reference/get_variance.html).

## References

McKelvey, R., Zavoina, W. (1975), "A Statistical Model for the Analysis
of Ordinal Level Dependent Variables", Journal of Mathematical Sociology
4, S. 103–120.

## Examples

``` r
## Dobson (1990) Page 93: Randomized Controlled Trial:
counts <- c(18, 17, 15, 20, 10, 20, 25, 13, 12) #
outcome <- gl(3, 1, 9)
treatment <- gl(3, 3)
model <- glm(counts ~ outcome + treatment, family = poisson())

r2_mckelvey(model)
#> McKelvey's R2 
#>     0.3776292 
```
