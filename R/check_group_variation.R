#' Check variables for within- and/or between-group variation
#'
#' `check_group_variation()` checks if variables a within- and/or between-effect,
#' i.e. if they vary within or between certain groups.
#'
#' @param x A data frame.
#' @param select Character vector (or formula) with names of variables to select
#'   that should be checked. If `NULL`, selects all variables (except those in
#'   `by`).
#' @param by Character vector (or formula) with the name of the variable that
#'   indicates the group- or cluster-ID. For cross-classified or nested designs,
#'   `by` can also identify two or more variables as group- or cluster-IDs.
#' @param include_by When there is more than one grouping variable, should they
#'   be check against each other?
#' @param numeric_as_factor Should numeric variables be tested as factors?
#' @param num_tolerance The amount of variation (calculated by `var()`, i.e. the
#' variance of a variable) that is tolerated to indicate no within- or
#' between-effect.
#' @param fct_tolerance How should a non-numeric variable be identified as
#'   varying only "within" a grouping variable? Options are:
#'   - `"crossed"` - if all groups have all unique values of X.
#'   - `"balanced"` - if all groups have all unique values of X, _with equal frequency_.

#'
#' @details
#' This function attempt to identify the hierarchical design of a dataset with
#' respect to grouping variables (`by`).
#'
#' ## Numeric variables
#' Numeric variables are portioned via [`datawizard::demean()`] to their within-
#' and between-group components. Then, the variance for each variable's within-
#' and between-group component is calculated. Variable with within-group
#' variance larger than `num_tolerance` are labeled as _within_, variable with
#' between-group variance larger than `num_tolerance` are labeled as _between_,
#' and variables with both variances larger than `num_tolerance` are labeled as
#' _both_.
#' \cr\cr
#' Setting `numeric_as_factor = TRUE` causes numeric variables to be tested
#' using the following criteria.
#'
#' ## Non-numeric variables
#' These variables can have one of the following 4 labels:
#' - _between_ - the variable is fixed (has exactly one unique, constant value) for each group.
#' - _nested_ - the variable varies within each group, with each group having their own set of
#'   unique levels of the variable.
#' - _within_ - the variable is _crossed_ with the grouping variable - each value appear
#'   within each group. The `fct_tolerance` argument controls if full balance is also required.
#' - _both_ - the variable is partially nested within the grouping variable (or, when
#'   `fct_tolerance = "balanced"` the variable is fully crossed, but not perfectly balanced).
#'
#' @return A data frame with group, variable, and type columns.
#'
#' @examples
#' @export
check_group_variation <- function(x,
                                  select = NULL,
                                  by = NULL,
                                  include_by = FALSE,
                                  numeric_as_factor = FALSE,
                                  num_tolerance = 1e-4,
                                  fct_tolerance = "crossed") {
  insight::check_if_installed("datawizard", minimum_version = "0.12.0")

  if (inherits(select, "formula")) {
    select <- all.vars(select)
  }
  if (inherits(by, "formula")) {
    by <- all.vars(by)
  }

  # sanity check
  if (is.null(by)) {
    insight::format_error("Please provide the group variable using `by`.")
  }
  if (!all(by %in% colnames(x))) {
    insight::format_error(
      "The variable(s) speciefied in `by` were not found in the data."
    )
  }

  # select all, if not given
  if (is.null(select)) {
    select <- setdiff(colnames(x), by)
  }

  if (include_by && (length(by) > 1L)) {
    select <- c(by, select)
  }

  if (numeric_as_factor) {
    x[select] <- lapply(x[select], as.factor)
  }

  # create all combinations that should be checked
  combinations <- expand.grid(
    variable = select,
    group = by,
    stringsAsFactors = FALSE
  )
  combinations <- combinations[combinations$variable != combinations$group, ]
  combinations$type <- NA_character_

  # initialize lists
  for (i in seq_len(nrow(combinations))) {
    combinations[i, "type"] <- .check_nested(
      x,
      combinations[i, "group"],
      combinations[i, "variable"],
      num_tolerance = num_tolerance,
      fct_tolerance = fct_tolerance
    )
  }

  combinations <- datawizard::data_relocate(
    combinations,
    select = "group",
    before = "variable"
  )
  class(combinations) <- c("check_group_variation", class(combinations))
  combinations
}


#' @export
print.check_group_variation <- function(x, ...) {
  x_orig <- x

  cap <- "Check group variation"
  by <- "group"

  if (insight::n_unique(x$group) == 1L) {
    x$group <- NULL
    cap <- sprintf("Check %s variation", x_orig$group[1])
    by <- NULL
  }

  cat(insight::export_table(x, caption = c(cap, "blue"), by = by, ...))

  return(invisible(x_orig))
}


# utils -------------------------------------------------------------

.check_nested <- function(data, by, predictor, ...) {
  if (insight::n_unique(data[[predictor]]) == 1L) {
    return(NA_character_)
  }

  UseMethod(".check_nested", data[[predictor]])
}


.check_nested.numeric <- function(data, by, predictor, num_tolerance = 1e-05, ...) {
  # demean predictor
  d <- datawizard::demean(
    data,
    select = predictor,
    by = by,
    verbose = FALSE,
    add_attributes = FALSE
  )

  # get new names
  within_name <- paste0(predictor, "_within")
  between_name <- paste0(predictor, "_between")

  is_between <- stats::var(d[[between_name]], na.rm = TRUE) > num_tolerance
  is_within <- stats::var(d[[within_name]], na.rm = TRUE) > num_tolerance
  is_both <- is_between && is_within

  if (is_both) {
    return("both")
  }
  if (is_between) {
    return("between")
  }
  if (is_within) {
    return("within")
  }

  NA_character_
}


.check_nested.default <- function(data, by, predictor, fct_tolerance = c("crossed", "balanced"), ...) {
  fct_tolerance <- match.arg(fct_tolerance)

  group <- data[[by]]
  variable <- data[[predictor]]

  complete <- stats::complete.cases(group, variable)
  group <- as.factor(group[complete])
  variable <- variable[complete]

  # Is the variable fixed for each group?
  n_uniques <- tapply(variable, group, insight::n_unique)
  is_between <- all(n_uniques == 1L)
  if (is_between) {
    return("between")
  }

  # Is the variable nested within each group?
  if (insight::check_if_installed("Matrix", reason = "for checking nested designs")) {
    # code from lme4::isNested
    f1 <- as.factor(variable)
    f2 <- as.factor(group)
    k <- length(levels(f1))
    sm <- methods::as(
      methods::new("ngTMatrix",
        i = as.integer(f2) - 1L,
        j = as.integer(f1) - 1L,
        Dim = c(length(levels(f2)), k)
      ),
      "CsparseMatrix"
    )
    if (all(sm@p[2:(k + 1L)] - sm@p[1:k] <= 1L)) {
      return("nested")
    }
  }

  # If each group has a different number of unique values,
  # then it is partially nested/crossed.
  if (!insight::has_single_value(n_uniques)) {
    return("both")
  }

  # Is the variable crossed?
  variable_levels <- unique(variable)
  has_all <- tapply(variable, group, function(v) all(variable_levels %in% v))
  if (!all(has_all)) {
    return("both")
  }

  if (fct_tolerance == "crossed") {
    return("within")
  }

  # Is the variable crossed and balanced?
  tab <- table(variable, group)
  is_balanced <- all(apply(tab, 2, insight::has_single_value))
  if (!is_balanced) {
    return("both")
  }

  return("within")
}
