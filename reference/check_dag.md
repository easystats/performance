# Check correct model adjustment for identifying causal effects

The purpose of `check_dag()` is to build, check and visualize your model
based on directed acyclic graphs (DAG). The function checks if a model
is correctly adjusted for identifying specific relationships of
variables, especially directed (maybe also "causal") effects for given
exposures on an outcome. In case of incorrect adjustments, the function
suggests the minimal required variables that should be adjusted for
(sometimes also called "controlled for"), i.e. variables that *at least*
need to be included in the model. Depending on the goal of the analysis,
it is still possible to add more variables to the model than just the
minimally required adjustment sets.

`check_dag()` is a convenient wrapper around
[`ggdag::dagify()`](https://r-causal.github.io/ggdag/reference/dagify.html),
[`dagitty::adjustmentSets()`](https://rdrr.io/pkg/dagitty/man/adjustmentSets.html)
and
[`dagitty::adjustedNodes()`](https://rdrr.io/pkg/dagitty/man/VariableStatus.html)
to check correct adjustment sets. It returns a **dagitty** object that
can be visualized with
[`plot()`](https://rdrr.io/r/graphics/plot.default.html). `as.dag()` is
a small convenient function to return the dagitty-string, which can be
used for the online-tool from the dagitty-website.

## Usage

``` r
check_dag(
  ...,
  outcome = NULL,
  exposure = NULL,
  adjusted = NULL,
  latent = NULL,
  effect = "all",
  coords = NULL
)

as.dag(x, ...)
```

## Arguments

- ...:

  One or more formulas, which are converted into **dagitty** syntax.
  First element may also be model object. If a model objects is
  provided, its formula is used as first formula, and all independent
  variables will be used for the `adjusted` argument. See 'Details' and
  'Examples'.

- outcome:

  Name of the dependent variable (outcome), as character string or as
  formula. Must be a valid name from the formulas provided in `...`. If
  not set, the first dependent variable from the formulas is used.

- exposure:

  Name of the exposure variable (as character string or formula), for
  which the direct and total causal effect on the `outcome` should be
  checked. Must be a valid name from the formulas provided in `...`. If
  not set, the first independent variable from the formulas is used.

- adjusted:

  A character vector or formula with names of variables that are
  adjusted for in the model, e.g. `adjusted = c("x1", "x2")` or
  `adjusted = ~ x1 + x2`. If a model object is provided in `...`, any
  values in `adjusted` will be overwritten by the model's independent
  variables.

- latent:

  A character vector with names of latent variables in the model.

- effect:

  Character string, indicating which effect to check. Can be `"all"`
  (default), `"total"`, or `"direct"`.

- coords:

  Coordinates of the variables when plotting the DAG. The coordinates
  can be provided in three different ways:

  - a list with two elements, `x` and `y`, which both are named vectors
    of numerics. The names correspond to the variable names in the DAG,
    and the values for `x` and `y` indicate the x/y coordinates in the
    plot.

  - a list with elements that correspond to the variables in the DAG.
    Each element is a numeric vector of length two with x- and
    y-coordinate.

  - a data frame with three columns: `x`, `y` and `name` (which contains
    the variable names).

  See 'Examples'.

- x:

  An object of class `check_dag`, as returned by `check_dag()`.

## Value

An object of class `check_dag`, which can be visualized with
[`plot()`](https://rdrr.io/r/graphics/plot.default.html). The returned
object also inherits from class `dagitty` and thus can be used with all
functions from the **ggdag** and **dagitty** packages.

## Specifying the DAG formulas

The formulas have following syntax:

- One-directed paths: On the *left-hand-side* is the name of the
  variables where causal effects point to (direction of the arrows, in
  dagitty-language). On the *right-hand-side* are all variables where
  causal effects are assumed to come from. For example, the formula
  `Y ~ X1 + X2`, paths directed from both `X1` and `X2` to `Y` are
  assumed.

- Bi-directed paths: Use `~~` to indicate bi-directed paths. For
  example, `Y ~~ X` indicates that the path between `Y` and `X` is
  bi-directed, and the arrow points in both directions. Bi-directed
  paths often indicate unmeasured cause, or unmeasured confounding, of
  the two involved variables.

## Minimally required adjustments

The function checks if the model is correctly adjusted for identifying
the direct and total effects of the exposure on the outcome. If the
model is correctly specified, no adjustment is needed to estimate the
direct effect. If the model is not correctly specified, the function
suggests the minimally required variables that should be adjusted for.
The function distinguishes between direct and total effects, and checks
if the model is correctly adjusted for both. If the model is cyclic, the
function stops and suggests to remove cycles from the model.

Note that it sometimes could be necessary to try out different
combinations of suggested adjustments, because `check_dag()` can not
always detect whether *at least* one of several variables is required,
or whether adjustments should be done for *all* listed variables. It can
be useful to copy the dagitty-code (using `as.dag()`, which prints the
dagitty-string into the console) into the dagitty-website and play
around with different adjustments.

## Direct and total effects

The direct effect of an exposure on an outcome is the effect that is not
mediated by any other variable in the model. The total effect is the sum
of the direct and indirect effects. The function checks if the model is
correctly adjusted for identifying the direct and total effects of the
exposure on the outcome.

## Why are DAGs important - the Table 2 fallacy

Correctly thinking about and identifying the relationships between
variables is important when it comes to reporting coefficients from
regression models that mutually adjust for "confounders" or include
covariates. Different coefficients might have different interpretations,
depending on their relationship to other variables in the model.
Sometimes, a regression coefficient represents the direct effect of an
exposure on an outcome, but sometimes it must be interpreted as total
effect, due to the involvement of mediating effects. This problem is
also called "Table 2 fallacy" (*Westreich and Greenland 2013*). DAG
helps visualizing and thereby focusing the relationships of variables in
a regression model to detect missing adjustments or over-adjustment.

## References

- Rohrer, J. M. (2018). Thinking clearly about correlations and
  causation: Graphical causal models for observational data. Advances in
  Methods and Practices in Psychological Science, 1(1), 27–42.
  [doi:10.1177/2515245917745629](https://doi.org/10.1177/2515245917745629)

- Westreich, D., & Greenland, S. (2013). The Table 2 Fallacy: Presenting
  and Interpreting Confounder and Modifier Coefficients. American
  Journal of Epidemiology, 177(4), 292–298.
  [doi:10.1093/aje/kws412](https://doi.org/10.1093/aje/kws412)

## Examples

``` r
# no adjustment needed
check_dag(
  y ~ x + b,
  outcome = "y",
  exposure = "x"
)
#> # Check for correct adjustment sets
#> - Outcome: y
#> - Exposure: x
#> 
#> Identification of direct and total effects
#> 
#> Model is correctly specified.
#> No adjustment needed to estimate the direct and total effect of `x` on `y`.
#> 

# incorrect adjustment
dag <- check_dag(
  y ~ x + b + c,
  x ~ b,
  outcome = "y",
  exposure = "x"
)
dag
#> # Check for correct adjustment sets
#> - Outcome: y
#> - Exposure: x
#> 
#> Identification of direct and total effects
#> 
#> Incorrectly adjusted!
#> To estimate the direct and total effect, at least adjust for `b`. Currently, the model does not adjust for any variables.
#> 
plot(dag)


# After adjusting for `b`, the model is correctly specified
dag <- check_dag(
  y ~ x + b + c,
  x ~ b,
  outcome = "y",
  exposure = "x",
  adjusted = "b"
)
dag
#> # Check for correct adjustment sets
#> - Outcome: y
#> - Exposure: x
#> - Adjustment: b
#> 
#> Identification of direct and total effects
#> 
#> Model is correctly specified.
#> All minimal sufficient adjustments to estimate the direct and total effect were done.
#> 

# using formula interface for arguments "outcome", "exposure" and "adjusted"
check_dag(
  y ~ x + b + c,
  x ~ b,
  outcome = ~y,
  exposure = ~x,
  adjusted = ~ b + c
)
#> # Check for correct adjustment sets
#> - Outcome: y
#> - Exposure: x
#> - Adjustments: b and c
#> 
#> Identification of direct and total effects
#> 
#> Model is correctly specified.
#> All minimal sufficient adjustments to estimate the direct and total effect were done.
#> 

# if not provided, "outcome" is taken from first formula, same for "exposure"
# thus, we can simplify the above expression to
check_dag(
  y ~ x + b + c,
  x ~ b,
  adjusted = ~ b + c
)
#> # Check for correct adjustment sets
#> - Outcome: y
#> - Exposure: x
#> - Adjustments: b and c
#> 
#> Identification of direct and total effects
#> 
#> Model is correctly specified.
#> All minimal sufficient adjustments to estimate the direct and total effect were done.
#> 

# use specific layout for the DAG
dag <- check_dag(
  score ~ exp + b + c,
  exp ~ b,
  outcome = "score",
  exposure = "exp",
  coords = list(
    # x-coordinates for all nodes
    x = c(score = 5, exp = 4, b = 3, c = 3),
    # y-coordinates for all nodes
    y = c(score = 3, exp = 3, b = 2, c = 4)
  )
)
plot(dag)


# alternative way of providing the coordinates
dag <- check_dag(
  score ~ exp + b + c,
  exp ~ b,
  outcome = "score",
  exposure = "exp",
  coords = list(
    # x/y coordinates for each node
    score = c(5, 3),
    exp = c(4, 3),
    b = c(3, 2),
    c = c(3, 4)
  )
)
plot(dag)


# Objects returned by `check_dag()` can be used with "ggdag" or "dagitty"
ggdag::ggdag_status(dag)


# Using a model object to extract information about outcome,
# exposure and adjusted variables
data(mtcars)
m <- lm(mpg ~ wt + gear + disp + cyl, data = mtcars)
dag <- check_dag(
  m,
  wt ~ disp + cyl,
  wt ~ am
)
dag
#> # Check for correct adjustment sets
#> - Outcome: mpg
#> - Exposure: wt
#> - Adjustments: cyl, disp and gear
#> 
#> Identification of direct and total effects
#> 
#> Model is correctly specified.
#> All minimal sufficient adjustments to estimate the direct and total effect were done.
#> 
plot(dag)
```
