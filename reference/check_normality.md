# Check model for (non-)normality of residuals.

Check model for (non-)normality of residuals.

## Usage

``` r
check_normality(x, ...)

# S3 method for class 'merMod'
check_normality(x, effects = "fixed", ...)
```

## Arguments

- x:

  A model object.

- ...:

  Currently not used.

- effects:

  Should normality for residuals (`"fixed"`) or random effects
  (`"random"`) be tested? Only applies to mixed-effects models. May be
  abbreviated.

## Value

The p-value of the test statistics. A p-value \< 0.05 indicates a
significant deviation from normal distribution.

## Details

`check_normality()` calls
[`stats::shapiro.test`](https://rdrr.io/r/stats/shapiro.test.html) and
checks the standardized residuals (or studentized residuals for mixed
models) for normal distribution. Note that this formal test almost
always yields significant results for the distribution of residuals and
visual inspection (e.g. Q-Q plots) are preferable. For generalized
linear models, no formal statistical test is carried out. Rather,
there's only a [`plot()`](https://rdrr.io/r/graphics/plot.default.html)
method for GLMs. This plot shows a half-normal Q-Q plot of the absolute
value of the standardized deviance residuals is shown (in line with
changes in `plot.lm()` for R 4.3+).

## Note

For mixed-effects models, studentized residuals, and *not* standardized
residuals, are used for the test. There is also a
[[`plot()`](https://rdrr.io/r/graphics/plot.default.html)-method](https://easystats.github.io/see/articles/performance.html)
implemented in the [**see**-package](https://easystats.github.io/see/).

## See also

[`see::plot.see_check_normality()`](https://easystats.github.io/see/reference/plot.see_check_normality.html)
for options to customize the plot.

## Examples

``` r
m <<- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
check_normality(m)
#> OK: residuals appear as normally distributed (p = 0.230).
#> 

# plot results
x <- check_normality(m)
plot(x)


# \donttest{
# QQ-plot
plot(check_normality(m), type = "qq")


# PP-plot
plot(check_normality(m), type = "pp")

# }
```
