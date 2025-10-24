# Model summary for k-means clustering

Model summary for k-means clustering

## Usage

``` r
# S3 method for class 'kmeans'
model_performance(model, verbose = TRUE, ...)
```

## Arguments

- model:

  Object of type `kmeans`.

- verbose:

  Toggle off warnings.

- ...:

  Arguments passed to or from other methods.

## Examples

``` r
# a 2-dimensional example
x <- rbind(
  matrix(rnorm(100, sd = 0.3), ncol = 2),
  matrix(rnorm(100, mean = 1, sd = 0.3), ncol = 2)
)
colnames(x) <- c("x", "y")
model <- kmeans(x, 2)
model_performance(model)
#> # Indices of model performance
#> 
#> Sum_Squares_Total | Sum_Squares_Within | Sum_Squares_Between | Iterations
#> -------------------------------------------------------------------------
#> 73.097            |             16.232 |              56.865 |          1
```
