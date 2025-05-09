#' @title Check variables for within- and/or between-group variation
#' @name check_group_variation
#'
#' @description
#' Checks if variables vary within and/or between levels of grouping variables.
#' This function can be used to infer the hierarchical Design of a given
#' dataset, or detect any predictors that might cause heterogeneity bias (_Bell
#' and Jones, 2015_). Use `summary()` on the output if you are mainly interested
#' if and which predictors are possibly affected by heterogeneity bias.
#'
#' @param x A data frame or a mixed model. See details and examples.
#' @param select Character vector (or formula) with names of variables to select
#' that should be checked. If `NULL`, selects all variables (except those in
#' `by`).
#' @param by Character vector (or formula) with the name of the variable that
#' indicates the group- or cluster-ID. For cross-classified or nested designs,
#' `by` can also identify two or more variables as group- or cluster-IDs.
#' @param include_by When there is more than one grouping variable, should they
#' be check against each other?
#' @param numeric_as_factor Should numeric variables be tested as factors?
#' @param tolerance_numeric The amount of variation (calculated by `var()`, i.e.
#' the variance of a variable) that is tolerated to indicate no within- or
#' between-effect.
#' @param tolerance_factor How should a non-numeric variable be identified as
#' varying only "within" a grouping variable? Options are:
#' - `"crossed"` - if all groups have all unique values of X.
#' - `"balanced"` - if all groups have all unique values of X, _with equal
#'   frequency_.
#' @param ... Arguments passed to other methods
#'
#' @details
#' This function attempt to identify the hierarchical design of a dataset with
#' respect to grouping variables (`by`). If `x` is a (mixed effect) model, the
#' variability of the fixed effects predictors are checked with respect to the
#' random grouping variables.
#'
#' ## Numeric variables
#' Numeric variables are portioned via [`datawizard::demean()`] to their within-
#' and between-group components. Then, the variance for each variable's within-
#' and between-group component is calculated. Variables with within-group
#' variance larger than `tolerance_numeric` are labeled as _within_, variables
#' with a between-group variance larger than `tolerance_numeric` are labeled as
#' _between_, and variables with both variances larger than `tolerance_numeric`
#' are labeled as _both_.
#'
#' Setting `numeric_as_factor = TRUE` causes numeric variables to be tested
#' using the following criteria.
#'
#' ## Non-numeric variables
#' These variables can have one of the following three labels:
#' - _between_ - the variable is fixed (has exactly one unique, constant value)
#'   for each group.
#' - _within_ - the variable is _crossed_ with the grouping variable - each
#'   value appear within each group. The `tolerance_factor` argument controls if
#'   full balance is also required.
#' - _both_ - the variable is partially nested within the grouping variable (or,
#'   when `tolerance_factor = "balanced"` the variable is fully crossed, but not
#'   perfectly balanced).
#'
#' Additionally, the design of non-numeric variables is also checked to see if
#' they are nested within the groups or is they are crossed. This is indicated
#' in the column `Design`.
#'
#' ## Heterogeneity bias
#' Variables that vary both within and between groups can cause a heterogeneity
#' bias (_Bell and Jones, 2015_). It is recommended to center (person-mean
#' centering) those variables to avoid this bias. See [`datawizard::demean()`]
#' for further details. Use `summary()` to get a short text result that indicates
#' if and which predictors are possibly affected by heterogeneity bias.
#'
#' @return A data frame with Group, Variable, Variation and Design columns.
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
#' @examples
#' data(npk)
#' check_group_variation(npk, by = "block")
#'
#' data(iris)
#' check_group_variation(iris, by = "Species")
#'
#' data(ChickWeight)
#' check_group_variation(ChickWeight, by = "Chick")
#'
#' # A subset of mlmRev::egsingle
#' egsingle <- data.frame(
#'   schoolid = factor(rep(c("2020", "2820"), times = c(18, 6))),
#'   lowinc = rep(c(TRUE, FALSE), times = c(18, 6)),
#'   childid = factor(rep(
#'     c("288643371", "292020281", "292020361", "295341521"),
#'     each = 6
#'   )),
#'   female = rep(c(TRUE, FALSE), each = 12),
#'   year = rep(1:6, times = 4),
#'   math = c(
#'     -3.068, -1.13, -0.921, 0.463, 0.021, 2.035,
#'     -2.732, -2.097, -0.988, 0.227, 0.403, 1.623,
#'     -2.732, -1.898, -0.921, 0.587, 1.578, 2.3,
#'     -2.288, -2.162, -1.631, -1.555, -0.725, 0.097
#'   )
#' )
#'
#' result <- check_group_variation(
#'   egsingle,
#'   by = c("schoolid", "childid"),
#'   include_by = TRUE
#' )
#' result
#'
#' summary(result)
#'
#' @examplesIf insight::check_if_installed("lme4", quietly = TRUE)
#'
#' data(sleepstudy, package = "lme4")
#' check_group_variation(sleepstudy, select = "Days", by = "Subject")
#'
#' # Or
#' mod <- lme4::lmer(Reaction ~ Days + (Days | Subject), data = sleepstudy)
#' result <- check_group_variation(mod)
#' result
#'
#' summary(result)
#'
#' @export
check_group_variation <- function(x, ...) {
  UseMethod("check_group_variation")
}


#' @rdname check_group_variation
#' @export
check_group_variation.default <- function(x, ...) {
  if (!insight::is_model(x)) {
    insight::format_error("`x` must be a data frame or mixed model.")
  }

  by <- insight::find_random(x, split_nested = TRUE, flatten = TRUE)
  if (is.null(by)) {
    insight::format_error("Model is no mixed model. Please provide a mixed model, or a data frame and arguments `select` and `by`.")
  }
  my_data <- insight::get_data(x, source = "mf", verbose = FALSE)
  select <- insight::find_predictors(x, effects = "fixed", component = "conditional", flatten = TRUE)

  check_group_variation(my_data, select = select, by = by, ...)
}


#' @rdname check_group_variation
#' @export
check_group_variation.data.frame <- function(x,
                                             select = NULL,
                                             by = NULL,
                                             include_by = FALSE,
                                             numeric_as_factor = FALSE,
                                             tolerance_numeric = 1e-4,
                                             tolerance_factor = "crossed",
                                             ...) {
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
      "The variable(s) specified in `by` were not found in the data."
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
    Variable = select,
    Group = by,
    stringsAsFactors = FALSE
  )
  combinations <- combinations[combinations$Variable != combinations$Group, ]
  combinations$Variation <- NA_character_
  combinations$Design <- NA_character_

  # initialize lists
  for (i in seq_len(nrow(combinations))) {
    combinations[i, c("Variation", "Design")] <- .check_nested(
      x,
      combinations[i, "Group"],
      combinations[i, "Variable"],
      tolerance_numeric = tolerance_numeric,
      tolerance_factor = tolerance_factor
    )
  }

  combinations <- datawizard::data_relocate(
    combinations,
    select = "Group",
    before = "Variable"
  )
  class(combinations) <- c("check_group_variation", class(combinations))
  combinations
}


# methods -------------------------------------------------------------

#' @export
print.check_group_variation <- function(x, ...) {
  x_orig <- x

  if (insight::n_unique(x$Group) == 1L) {
    x$Group <- NULL
    caption <- c(sprintf("Check %s variation", x_orig$Group[1]), "blue")
  } else {
    caption <- as.list(sprintf("Check %s variation", unique(x$Group)))
    caption <- lapply(caption, append, "blue")
    x <- split(x, factor(x$Group, levels = unique(x$Group)))
    x[] <- lapply(x, function(i) {
      i$Group <- NULL
      i
    })
  }

  cat(insight::export_table(x, caption = caption, ...))

  invisible(x_orig)
}


#' @export
print_html.check_group_variation <- function(x, ...) {
  x_orig <- x
  caption <- "Check group variation"

  if (insight::n_unique(x$group) == 1L) {
    x$group <- group_by <- NULL
  } else {
    group_by <- "group"
  }

  insight::export_table(x, caption = caption, by = group_by, format = "html", ...)
}


#' @export
summary.check_group_variation <- function(object, ...) {
  # TODO if more than one group, show which group(s)
  result <- unique(object$Variable[startsWith(object$Variation, "both")])

  if (length(result)) {
    insight::format_alert(paste(
      "Possible heterogeneity bias due to following predictors:",
      insight::color_text(toString(result), "red")
    ))
  } else {
    insight::format_alert(insight::color_text(
      "No predictor found that could cause heterogeneity bias.",
      "green"
    ))
  }
  invisible(result)
}


# utils -------------------------------------------------------------

#' @keywords internals
.check_nested <- function(data, by, predictor, ...) {
  if (insight::n_unique(data[[predictor]]) == 1L) {
    return(NA_character_)
  }

  UseMethod(".check_nested", data[[predictor]])
}

#' @keywords internals
.check_nested.numeric <- function(data, by, predictor, tolerance_numeric = 1e-05, ...) {
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

  is_between <- stats::var(d[[between_name]], na.rm = TRUE) > tolerance_numeric
  is_within <- stats::var(d[[within_name]], na.rm = TRUE) > tolerance_numeric
  is_both <- is_between && is_within

  if (is_both) {
    return(c("both", NA_character_))
  }
  if (is_between) {
    return(c("between", NA_character_))
  }
  if (is_within) {
    return(c("within", NA_character_))
  }

  NA_character_
}

#' @keywords internals
.check_nested.default <- function(data, by, predictor, tolerance_factor = "crossed", ...) {
  tolerance_factor <- insight::validate_argument(
    tolerance_factor,
    c("crossed", "balanced")
  )

  group <- data[[by]]
  variable <- data[[predictor]]

  complete <- stats::complete.cases(group, variable)
  group <- droplevels(as.factor(group[complete]))
  variable <- variable[complete]

  struct <- NA_character_

  # Is the variable nested within each group?
  if (insight::check_if_installed("Matrix", reason = "for checking nested designs")) {
    # code from lme4::isNested
    f1 <- as.factor(variable)
    k <- nlevels(f1)
    sm <- methods::as(
      methods::new("ngTMatrix",
        i = as.integer(group) - 1L,
        j = as.integer(f1) - 1L,
        Dim = c(nlevels(group), k)
      ),
      "CsparseMatrix"
    )
    if (all(sm@p[2:(k + 1L)] - sm@p[1:k] <= 1L)) {
      struct <- "nested"
    }
  }

  # Is the variable fixed for each group?
  n_uniques <- tapply(variable, group, insight::n_unique)
  is_between <- all(n_uniques == 1L)
  if (is_between) {
    return(c("between", struct))
  }

  # If each group has a different number of unique values,
  # then it is partially nested/crossed.
  if (!insight::has_single_value(n_uniques)) {
    return(c("both", struct))
  }

  # Is the variable crossed?
  variable_levels <- unique(variable)
  has_all <- tapply(variable, group, function(v) all(variable_levels %in% v))
  if (!all(has_all)) {
    return(c("both", struct))
  }

  if (tolerance_factor == "crossed") {
    return(c("within", "crossed"))
  }

  # Is the variable crossed and balanced?
  tab <- table(variable, group)
  is_balanced <- all(apply(tab, 2, insight::has_single_value))
  if (is_balanced) {
    return(c("within", "crossed"))
  }

  c("both", "crossed")
}
