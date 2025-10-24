# Check suitability of data for Factor Analysis (FA) with Bartlett's Test of Sphericity and KMO

This checks whether the data is appropriate for Factor Analysis (FA) by
running the Bartlett's Test of Sphericity and the Kaiser, Meyer, Olkin
(KMO) Measure of Sampling Adequacy (MSA). See **details** below for more
information about the interpretation and meaning of each test.

## Usage

``` r
check_factorstructure(x, n = NULL, ...)

check_kmo(x, n = NULL, ...)

check_sphericity_bartlett(x, n = NULL, ...)
```

## Arguments

- x:

  A data frame or a correlation matrix. If the latter is passed, `n`
  must be provided.

- n:

  If a correlation matrix was passed, the number of observations must be
  specified.

- ...:

  Arguments passed to or from other methods.

## Value

A list of lists of indices related to sphericity and KMO.

## Details

### Bartlett's Test of Sphericity

Bartlett's (1951) test of sphericity tests whether a matrix (of
correlations) is significantly different from an identity matrix (filled
with 0). It tests whether the correlation coefficients are all 0. The
test computes the probability that the correlation matrix has
significant correlations among at least some of the variables in a
dataset, a prerequisite for factor analysis to work.

While it is often suggested to check whether Bartlett’s test of
sphericity is significant before starting with factor analysis, one
needs to remember that the test is testing a pretty extreme scenario
(that all correlations are non-significant). As the sample size
increases, this test tends to be always significant, which makes it not
particularly useful or informative in well-powered studies.

### Kaiser, Meyer, Olkin (KMO)

*(Measure of Sampling Adequacy (MSA) for Factor Analysis.)*

Kaiser (1970) introduced a Measure of Sampling Adequacy (MSA), later
modified by Kaiser and Rice (1974). The Kaiser-Meyer-Olkin (KMO)
statistic, which can vary from 0 to 1, indicates the degree to which
each variable in a set is predicted without error by the other
variables.

A value of 0 indicates that the sum of partial correlations is large
relative to the sum correlations, indicating factor analysis is likely
to be inappropriate. A KMO value close to 1 indicates that the sum of
partial correlations is not large relative to the sum of correlations
and so factor analysis should yield distinct and reliable factors. It
means that patterns of correlations are relatively compact, and so
factor analysis should yield distinct and reliable factors. Values
smaller than 0.5 suggest that you should either collect more data or
rethink which variables to include.

Kaiser (1974) suggested that KMO \> .9 were marvelous, in the .80s,
meritorious, in the .70s, middling, in the .60s, mediocre, in the .50s,
miserable, and less than .5, unacceptable. Hair et al. (2006) suggest
accepting a value \> 0.5. Values between 0.5 and 0.7 are mediocre, and
values between 0.7 and 0.8 are good.

Variables with individual KMO values below 0.5 could be considered for
exclusion them from the analysis (note that you would need to re-compute
the KMO indices as they are dependent on the whole dataset).

## References

This function is a wrapper around the `KMO` and the `cortest.bartlett()`
functions in the **psych** package (Revelle, 2016).

- Revelle, W. (2016). How To: Use the psych package for Factor Analysis
  and data reduction.

- Bartlett, M. S. (1951). The effect of standardization on a Chi-square
  approximation in factor analysis. Biometrika, 38(3/4), 337-344.

- Kaiser, H. F. (1970). A second generation little jiffy. Psychometrika,
  35(4), 401-415.

- Kaiser, H. F., & Rice, J. (1974). Little jiffy, mark IV. Educational
  and psychological measurement, 34(1), 111-117.

- Kaiser, H. F. (1974). An index of factorial simplicity. Psychometrika,
  39(1), 31-36.

## See also

[`check_clusterstructure()`](https://easystats.github.io/performance/reference/check_clusterstructure.md).

## Examples

``` r
library(performance)

check_factorstructure(mtcars)
#> # Is the data suitable for Factor Analysis?
#> 
#> 
#>   - Sphericity: Bartlett's test of sphericity suggests that there is sufficient significant correlation in the data for factor analysis (Chisq(55) = 408.01, p < .001).
#>   - KMO: The Kaiser, Meyer, Olkin (KMO) overall measure of sampling adequacy suggests that data seems appropriate for factor analysis (KMO = 0.83). The individual KMO scores are: mpg (0.93), cyl (0.90), disp (0.76), hp (0.84), drat (0.95), wt (0.74), qsec (0.74), vs (0.91), am (0.88), gear (0.85), carb (0.62).

# One can also pass a correlation matrix
r <- cor(mtcars)
check_factorstructure(r, n = nrow(mtcars))
#> # Is the data suitable for Factor Analysis?
#> 
#> 
#>   - Sphericity: Bartlett's test of sphericity suggests that there is sufficient significant correlation in the data for factor analysis (Chisq(55) = 408.01, p < .001).
#>   - KMO: The Kaiser, Meyer, Olkin (KMO) overall measure of sampling adequacy suggests that data seems appropriate for factor analysis (KMO = 0.83). The individual KMO scores are: mpg (0.93), cyl (0.90), disp (0.76), hp (0.84), drat (0.95), wt (0.74), qsec (0.74), vs (0.91), am (0.88), gear (0.85), carb (0.62).
```
