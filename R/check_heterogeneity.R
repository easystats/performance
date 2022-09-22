#' Check model predictor for heterogeneity bias
#'
#' `check_heterogeneity()` checks if model predictors or variables may
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
#' For further details, see documentation for `?datawizard::demean`.
#'
#' @note
#' This function will be removed in a future update. Please use
#' `performance::check_heterogeneity_bias()`.
#'
#' @export
check_heterogeneity <- function(x, select = NULL, group = NULL) {
  .Deprecated("performance::check_heterogeneity_bias()")

  if (insight::is_model(x)) {
    group <- insight::find_random(x, split_nested = TRUE, flatten = TRUE)
    if (is.null(group)) {
      stop(insight::format_message(
        "Model is no mixed model. Please provide a mixed model, or a data frame and arguments 'select' and 'group'."
      ), call. = FALSE)
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

  unique_groups <- insight::n_unique(data[[group]])
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

  out <- unname(unlist(datawizard::compact_list(result)))

  if (is.null(out)) {
    message("No predictor found that could cause heterogeneity bias.")
    return(invisible(NULL))
  }

  class(out) <- c("check_heterogeneity", class(out))

  out
}




#' @export
print.check_heterogeneity <- function(x, ...) {
  cat("Possible heterogeneity bias due to following predictors: ")
  insight::print_color(paste(x, collapse = ", "), "red")
  cat("\n")
  invisible(x)
}
