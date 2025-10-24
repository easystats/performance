# Multivariate R2

Calculates two multivariate R2 values for multivariate linear
regression.

## Usage

``` r
r2_mlm(model, ...)
```

## Arguments

- model:

  Multivariate linear regression model.

- ...:

  Currently not used.

## Value

A named vector with the R2 values.

## Details

The two indexes returned summarize model fit for the set of predictors
given the system of responses. As compared to the default
[r2](https://easystats.github.io/performance/reference/r2.md) index for
multivariate linear models, the indexes returned by this function
provide a single fit value collapsed across all responses.

The two returned indexes were proposed by *Van den Burg and Lewis
(1988)* as an extension of the metrics proposed by *Cramer and
Nicewander (1979)*. Of the numerous indexes proposed across these two
papers, only two metrics, the \\R\_{xy}\\ and \\P\_{xy}\\, are
recommended for use by *Azen and Budescu (2006)*.

For a multivariate linear regression with \\p\\ predictors and \\q\\
responses where \\p \> q\\, the \\R\_{xy}\\ index is computed as:

\$\$R\_{xy} = 1 - \prod\_{i=1}^p (1 - \rho_i^2)\$\$

Where \\\rho\\ is a canonical variate from a [canonical
correlation](https://rdrr.io/r/stats/cancor.html) between the predictors
and responses. This metric is symmetric and its value does not change
when the roles of the variables as predictors or responses are swapped.

The \\P\_{xy}\\ is computed as:

\$\$P\_{xy} = \frac{q -
trace(\bf{S}\_{\bf{YY}}^{-1}\bf{S}\_{\bf{YY\|X}})}{q}\$\$

Where \\\bf{S}\_{\bf{YY}}\\ is the matrix of response covariances and
\\\bf{S}\_{\bf{YY\|X}}\\ is the matrix of residual covariances given the
predictors. This metric is asymmetric and can change depending on which
variables are considered predictors versus responses.

## References

- Azen, R., & Budescu, D. V. (2006). Comparing predictors in
  multivariate regression models: An extension of dominance analysis.
  Journal of Educational and Behavioral Statistics, 31(2), 157-180.

- Cramer, E. M., & Nicewander, W. A. (1979). Some symmetric, invariant
  measures of multivariate association. Psychometrika, 44, 43-54.

- Van den Burg, W., & Lewis, C. (1988). Some properties of two measures
  of multivariate association. Psychometrika, 53, 109-122.

## Author

Joseph Luchman

## Examples

``` r
model <- lm(cbind(qsec, drat) ~ wt + mpg + cyl, data = mtcars)
r2_mlm(model)
#>  Symmetric Rxy Asymmetric Pxy 
#>      0.8573111      0.5517522 

model_swap <- lm(cbind(wt, mpg, cyl) ~ qsec + drat, data = mtcars)
r2_mlm(model_swap)
#>  Symmetric Rxy Asymmetric Pxy 
#>      0.8573111      0.3678348 
```
