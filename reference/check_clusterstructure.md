# Check suitability of data for clustering

This checks whether the data is appropriate for clustering using the
Hopkins' H statistic of given data. If the value of Hopkins statistic is
close to 0 (below 0.5), then we can reject the null hypothesis and
conclude that the dataset is significantly clusterable. A value for H
lower than 0.25 indicates a clustering tendency at the `90%` confidence
level. The visual assessment of cluster tendency (VAT) approach (Bezdek
and Hathaway, 2002) consists in investigating the heatmap of the ordered
dissimilarity matrix. Following this, one can potentially detect the
clustering tendency by counting the number of square shaped blocks along
the diagonal.

## Usage

``` r
check_clusterstructure(x, standardize = TRUE, distance = "euclidean", ...)
```

## Arguments

- x:

  A data frame.

- standardize:

  Standardize the data frame before clustering (default).

- distance:

  Distance method used. Other methods than "euclidean" (default) are
  exploratory in the context of clustering tendency. See
  [`stats::dist()`](https://rdrr.io/r/stats/dist.html) for list of
  available methods.

- ...:

  Arguments passed to or from other methods.

## Value

The H statistic (numeric)

## References

- Lawson, R. G., & Jurs, P. C. (1990). New index for clustering tendency
  and its application to chemical problems. Journal of chemical
  information and computer sciences, 30(1), 36-41.

- Bezdek, J. C., & Hathaway, R. J. (2002, May). VAT: A tool for visual
  assessment of (cluster) tendency. In Proceedings of the 2002
  International Joint Conference on Neural Networks. IJCNN02 (3),
  2225-2230. IEEE.

## See also

[`check_kmo()`](https://easystats.github.io/performance/reference/check_factorstructure.md),
[`check_sphericity_bartlett()`](https://easystats.github.io/performance/reference/check_factorstructure.md)
and
[`check_factorstructure()`](https://easystats.github.io/performance/reference/check_factorstructure.md).

## Examples

``` r
# \donttest{
library(performance)
check_clusterstructure(iris[, 1:4])
#> # Clustering tendency
#> 
#> The dataset is suitable for clustering (Hopkins' H = 0.19).
#> 
plot(check_clusterstructure(iris[, 1:4]))

# }
```
