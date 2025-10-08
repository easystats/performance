#' @title Check model predictor for heterogeneity bias *(Deprecated)*
#' @name check_heterogeneity_bias
#'
#' @description
#' `check_heterogeneity_bias()` checks if model predictors or variables may
#' cause a heterogeneity bias, i.e. if variables have any within-group variance
#' (_Bell and Jones, 2015_).
#'
#' **We recommend using [check_group_variation()] instead, for a more detailed
#' and flexible examination of group-wise variability.**
#'
#' @param x A data frame or a mixed model object.
#' @param select Character vector (or formula) with names of variables to select
#'   that should be checked. If `x` is a mixed model object, this argument
#'   will be ignored.
#' @param by Character vector (or formula) with the name of the variable that
#'   indicates the group- or cluster-ID. For cross-classified or nested designs,
#'   `by` can also identify two or more variables as group- or cluster-IDs. If
#'   the data is nested and should be treated as such, set `nested = TRUE`. Else,
#'   if `by` defines two or more variables and `nested = FALSE`, a cross-classified
#'   design is assumed. If `x` is a model object, this argument will be ignored.
#'
#'   For nested designs, `by` can be:
#'   - a character vector with the name of the variable that indicates the
#'     levels, ordered from *highest* level to *lowest* (e.g.
#'     `by = c("L4", "L3", "L2")`.
#'   - a character vector with variable names in the format `by = "L4/L3/L2"`,
#'     where the levels are separated by `/`.
#'
#'   See also section _De-meaning for cross-classified designs_ and
#'   _De-meaning for nested designs_ in [`datawizard::demean()`].
#'
#' @param nested Logical, if `TRUE`, the data is treated as nested. If `FALSE`,
#'   the data is treated as cross-classified. Only applies if `by` contains more
#'   than one variable.
#'
#' @seealso
#' For further details, read the vignette
#' <https://easystats.github.io/parameters/articles/demean.html> and also
#' see documentation for [`datawizard::demean()`].
#'
#' For a more detailed and flexible examination of group-wise variability, see
#' [`check_group_variation()`].
#'
#' @references
#' - Bell A, Jones K. 2015. Explaining Fixed Effects: Random Effects
#'   Modeling of Time-Series Cross-Sectional and Panel Data. Political Science
#'   Research and Methods, 3(1), 133–153.
#'
#' @examples
#' data(iris)
#' iris$ID <- sample(1:4, nrow(iris), replace = TRUE) # fake-ID
#' check_heterogeneity_bias(iris, select = c("Sepal.Length", "Petal.Length"), by = "ID")
#' @export
check_heterogeneity_bias <- function(x, select = NULL, by = NULL, nested = FALSE) {
  insight::format_alert(
    "`check_heterogeneity_bias()` is deprecated. Please use `check_group_variation()` instead."
  )

  if (insight::is_model(x)) {
    by <- insight::find_random(x, split_nested = TRUE, flatten = TRUE)
    if (is.null(by)) {
      insight::format_error(
        "Model is no mixed model. Please provide a mixed model, or a data frame and arguments `select` and `by`."
      ) # nolint
    }
    my_data <- insight::get_data(x, source = "mf", verbose = FALSE)
    select <- insight::find_predictors(
      x,
      effects = "fixed",
      component = "conditional",
      flatten = TRUE
    )
  } else {
    if (inherits(select, "formula")) {
      select <- all.vars(select)
    }
    if (inherits(by, "formula")) {
      by <- all.vars(by)
    }
    my_data <- x
  }

  # sanity check
  if (is.null(by)) {
    insight::format_error("Please provide the group variable using `by`.")
  }
  if (!all(by %in% colnames(my_data))) {
    insight::format_error(
      "The variable(s) speciefied in `by` were not found in the data."
    )
  }

  # select all, if not given
  if (is.null(select)) {
    select <- setdiff(colnames(my_data), by)
  }

  # for nested designs?
  if (nested) {
    # separate level-indicators with "/", as supported by datawizard
    by <- paste(by, collapse = "/")
  }

  # create all combinations that should be checked
  combinations <- expand.grid(select, by[1])

  result <- Map(
    function(predictor, id) {
      # demean predictor

      d <- datawizard::demean(my_data, select = predictor, by = id, verbose = FALSE)

      # get new names
      within_name <- paste0(predictor, "_within")

      # check if any within-variable differs from zero. if yes, we have
      # a within-subject effect
      if (any(abs(d[[within_name]]) > 1e-5)) {
        predictor
      } else {
        NULL
      }
    },
    as.character(combinations[[1]]),
    by
  )

  out <- unlist(insight::compact_list(result), use.names = FALSE)

  if (is.null(out)) {
    message("No predictor found that could cause heterogeneity bias.")
    return(invisible(NULL))
  }

  class(out) <- c("check_heterogeneity_bias", class(out))

  out
}


#' @export
print.check_heterogeneity_bias <- function(x, ...) {
  cat("Possible heterogeneity bias due to following predictors: ")
  insight::print_color(toString(x), "red")
  cat("\n")
  invisible(x)
}
