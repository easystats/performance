#' Check model predictor for heterogeneity bias
#'
#' `check_heterogeneity_bias()` checks if model predictors or variables may
#' cause a heterogeneity bias, i.e. if variables have a within- and/or
#' between-effect.
#'
#' @param x A data frame or a mixed model object.
#' @param select Character vector (or formula) with names of variables to select
#'   that should be checked. If `x` is a mixed model object, this argument
#'   will be ignored.
#' @param group Character vector (or formula) with the name of the variable that
#'   indicates the group- or cluster-ID. If `x` is a model object, this
#'   argument will be ignored.
#'
#' @seealso
#' For further details, read the vignette
#' <https://easystats.github.io/parameters/articles/demean.html> and also
#' see documentation for `?datawizard::demean`.
#'
#' @examples
#' data(iris)
#' iris$ID <- sample(1:4, nrow(iris), replace = TRUE) # fake-ID
#' check_heterogeneity_bias(iris, select = c("Sepal.Length", "Petal.Length"), group = "ID")
#' @export
check_heterogeneity_bias <- function(x, select = NULL, group = NULL) {
  if (insight::is_model(x)) {
    group <- insight::find_random(x, split_nested = TRUE, flatten = TRUE)
    if (is.null(group)) {
      stop("Model is no mixed model. Please provide a mixed model, or a data frame and arguments 'select' and 'group'.")
    }
    data <- insight::get_data(x)
    select <- insight::find_predictors(x, effects = "fixed", component = "conditional", flatten = TRUE)
  } else {
    if (inherits(select, "formula")) {
      select <- all.vars(select)
    }
    if (inherits(group, "formula")) {
      group <- all.vars(group)
    }
    data <- x
  }

  unique_groups <- .n_unique(data[[group]])
  combinations <- expand.grid(select, group)

  result <- mapply(function(predictor, id) {
    # demean predictor
    d <- datawizard::demean(data, select = predictor, group = id, verbose = FALSE)

    # get new names
    within_name <- paste0(predictor, "_within")

    # check if any within-variable differs from zero. if yes, we have
    # a within-subject effect
    if (any(sum(abs(d[[within_name]]) > 1e-5, na.rm = TRUE) > 0)) {
      predictor
    } else {
      NULL
    }
  }, as.character(combinations[[1]]), as.character(combinations[[2]]), SIMPLIFY = FALSE)

  out <- unname(unlist(.compact_list(result)))

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
  insight::print_color(paste(x, collapse = ", "), "red")
  cat("\n")
  invisible(x)
}


#' @keywords internal
.n_unique <- function(x, na.rm = TRUE) {
  if (is.null(x)) {
    return(0)
  }
  if (isTRUE(na.rm)) x <- stats::na.omit(x)
  length(unique(x))
}
