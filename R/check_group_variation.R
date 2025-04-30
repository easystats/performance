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
check_group_variation <- function(x, select = NULL, by = NULL, nested = FALSE, tolerance = 1e-4) {
  insight::check_if_installed("datawizard", minimum_version = "0.12.0")

  if (inherits(select, "formula")) {
    select <- all.vars(select)
  }
  if (inherits(by, "formula")) {
    by <- all.vars(by)
  }
  my_data <- x

  # sanity check
  if (is.null(by)) {
    insight::format_error("Please provide the group variable using `by`.")
  }
  if (!all(by %in% colnames(x))) {
    insight::format_error("The variable(s) speciefied in `by` were not found in the data.")
  }

  # select all, if not given
  if (is.null(select)) {
    select <- setdiff(colnames(x), by)
  }

  # for nested designs?
  if (nested) {
    # separate level-indicators with "/", as supported by datawizard
    by <- paste(by, collapse = "/")
  }

  # create all combinations that should be checked
  combinations <- expand.grid(select, by[1])

  # initialize lists
  list_within <- list_between <- list_both <- NULL

  for (predictor in combinations[[1]]) {
    # demean predictor
    d <- datawizard::demean(my_data, select = predictor, by = by, verbose = FALSE, add_attributes = FALSE)

    # get new names
    within_name <- paste0(predictor, "_within")
    between_name <- paste0(predictor, "_between")

    if (var(d[[within_name]], na.rm = TRUE) > tolerance && var(d[[between_name]], na.rm = TRUE) > tolerance) {
      list_both <- c(list_both, predictor)
    } else if (var(d[[within_name]], na.rm = TRUE) > tolerance) {
      list_within <- c(list_within, predictor)
    } else if (var(d[[between_name]], na.rm = TRUE) > tolerance) {
      list_between <- c(list_between, predictor)
    }
  }

  out <- insight::compact_list(list(
    within = list_within,
    between = list_between,
    both = list_both
  ))

  if (is.null(out)) {
    insight::format_alert("No predictors found that either have within or between group variation.")
    return(invisible(NULL))
  }

  class(out) <- c("check_group_variation", class(out))

  out
}


#' @export
print.check_group_variation <- function(x, ...) {
  out <- as.data.frame(lapply(as.data.frame(do.call(cbind, x)), function(i) {
    i[duplicated(i)] <- NA_character_
    i
  }))
  cat(insight::export_table(out, caption = c("Check group variation", "blue")))
}
