#' Check model predictor for heterogeneity bias
#'
#' `check_heterogeneity_bias()` checks if model predictors or variables may
#' cause a heterogeneity bias, i.e. if variables have a within- and/or
#' between-effect (_Bell and Jones, 2015_).
#'
#' @param x A data frame or a mixed model object.
#' @param select Character vector (or formula) with names of variables to select
#'   that should be checked. If `x` is a mixed model object, this argument
#'   will be ignored.
#' @param by Character vector (or formula) with the name of the variable that
#'   indicates the group- or cluster-ID. If `x` is a model object, this
#'   argument will be ignored.
#' @param group Deprecated. Use `by` instead.
#'
#' @seealso
#' For further details, read the vignette
#' <https://easystats.github.io/parameters/articles/demean.html> and also
#' see documentation for [`datawizard::demean()`].
#'
#' @references
#' - Bell A, Jones K. 2015. Explaining Fixed Effects: Random Effects
#'   Modeling of Time-Series Cross-Sectional and Panel Data. Political Science
#'   Research and Methods, 3(1), 133–153.
#'
#' @examplesIf insight::check_if_installed("datawizard", minimum_version = "0.12.0", quietly = TRUE)
#' data(iris)
#' iris$ID <- sample(1:4, nrow(iris), replace = TRUE) # fake-ID
#' check_heterogeneity_bias(iris, select = c("Sepal.Length", "Petal.Length"), by = "ID")
#' @export
check_heterogeneity_bias <- function(x, select = NULL, by = NULL, group = NULL) {
  insight::check_if_installed("datawizard", minimum_version = "0.12.0")

  ## TODO: deprecate later
  if (!is.null(group)) {
    insight::format_warning("Argument `group` is deprecated and will be removed in a future release. Please use `by` instead.") # nolint
    by <- group
  }

  if (insight::is_model(x)) {
    by <- insight::find_random(x, split_nested = TRUE, flatten = TRUE)
    if (is.null(by)) {
      insight::format_error("Model is no mixed model. Please provide a mixed model, or a data frame and arguments `select` and `by`.") # nolint
    }
    my_data <- insight::get_data(x, source = "mf", verbose = FALSE)
    select <- insight::find_predictors(x, effects = "fixed", component = "conditional", flatten = TRUE)
  } else {
    if (inherits(select, "formula")) {
      select <- all.vars(select)
    }
    if (inherits(by, "formula")) {
      by <- all.vars(by)
    }
    my_data <- x
  }

  unique_groups <- .n_unique(my_data[[by]])
  combinations <- expand.grid(select, by)

  result <- Map(function(predictor, id) {
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
  }, as.character(combinations[[1]]), as.character(combinations[[2]]))

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


#' @keywords internal
.n_unique <- function(x, na.rm = TRUE) {
  if (is.null(x)) {
    return(0)
  }
  if (isTRUE(na.rm)) x <- x[!is.na(x)]
  length(unique(x))
}
