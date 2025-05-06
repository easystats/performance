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
#'   `by` can also identify two or more variables as group- or cluster-IDs. If
#'   the data is nested and should be treated as such, set `nested = TRUE`. Else,
#'   if `by` defines two or more variables and `nested = FALSE`, a cross-classified
#'   design is assumed.
#'
#'   For nested designs, `by` can be:
#'   - a character vector with the name of the variable that indicates the
#'     levels, ordered from *highest* level to *lowest* (e.g.
#'     `by = c("L4", "L3", "L2")`.
#'   - a character vector with variable names in the format `by = "L4/L3/L2"`,
#'     where the levels are separated by `/`.
#' @param nested Logical, if `TRUE`, the data is treated as nested. If `FALSE`,
#'   the data is treated as cross-classified. Only applies if `by` contains more
#'   than one variable.
#' @param tolerance The amount of variation (calculated by `var()`, i.e. the
#' variance of a variable) that is tolerated to indicate no within- or
#' between-effect.
#'
#' @details
#' This function calls [`datawizard::demean()`] to calculate the within- and
#' between-effects of variables specified in `select`, based on the groups
#' indicated in `by`. Then, the variance for each variable's within- and
#' between-effect is calculated. If the variance is larger than `tolerance`,
#' a within- or between-effect is detected.
#'
#' @return A list with at most three elements, `within`, `between`, and `both`,
#' where each element contains the name of variables that have one of these
#' effects.
#'
#' @examples
#' set.seed(1234)
#' dat <- data.frame(
#'   id = rep(letters, each = 3),
#'   between_num = rep(rnorm(26), each = 3),
#'   within1_num = rep(rnorm(3), times = 26),
#'   within2_num = rep(rnorm(3), times = 26),
#'   both_num = rnorm(3 * 26)
#' )
#' check_group_variation(
#'   dat,
#'   select = c("between_num", "within1_num", "within2_num", "both_num"),
#'   by = "id"
#' )
#' @export
check_group_variation <- function(
  x,
  select = NULL,
  by = NULL,
  tolerance = 1e-4,
  only_balanced = TRUE
) {
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
      tolerance = tolerance,
      only_balanced = only_balanced
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
  if (insight::n_unique(x$group) > 1L) {
    cat(insight::export_table(
      x,
      caption = c("Check group variation", "blue"),
      by = "group"
    ))
  } else {
    x_new <- x
    x_new$group <- NULL
    cat(insight::export_table(
      x_new,
      caption = c(sprintf("Check %s variation", x$group[1]), "blue")
    ))
  }

  return(invisible(x))
}


# utils -------------------------------------------------------------

.check_nested <- function(data, by, predictor, ...) {
  UseMethod(".check_nested", data[[predictor]])
}


.check_nested.numeric <- function(data, by, predictor, tolerance = 1e-05, ...) {
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

  is_between <- var(d[[between_name]], na.rm = TRUE) > tolerance
  is_within <- var(d[[within_name]], na.rm = TRUE) > tolerance
  is_both <- is_between && is_within

  if (is_both) return("both")
  if (is_between) return("between")
  if (is_within) return("within")
  NULL
}


.check_nested.default <- function(data, by, predictor, only_balanced = TRUE, ...) {
  group <- data[[by]]
  variable <- data[[predictor]]

  complete <- stats::complete.cases(group, variable)
  group <- as.factor(group[complete])
  variable <- variable[complete]

  n_uniques <- tapply(variable, group, insight::n_unique)
  is_between <- all(n_uniques == 1L)
  if (is_between) return("between")

  if (!insight::has_single_value(is_between)) return("both")

  variable_levels <- unique(variable)
  has_all <- tapply(variable, group, function(v) all(variable_levels %in% v))
  if (!all(has_all)) return("both")

  if (only_balanced) {
    tab <- table(variable, group)
    is_balanced <- all(apply(tab, 2, insight::has_single_value))
    if (!is_balanced) {
      return("both")
    }
  }
  return("within")
}
