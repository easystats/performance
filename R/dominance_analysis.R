#' @title Dominance Analysis
#' @name dominance_analysis
#' @inheritParams domir::domin
#'
#' @description Computes Dominance Analysis Statistics and Designations
#'
#' @param model A model object supported by `performance::r2()`. See 'Details'.
#' @param sets A (named) list of formula objects with no left hand
#' side/response.  A named list uses the name provided each element
#' as the label for the set.
#'
#' Predictors in each formula are bound together as a set in the dominance
#' analysis and dominance statistics and designations are computed for
#' the predictors together.  Predictors in `sets` must be present in the model
#' submitted to the `model` argument and cannot be in the `all` argument.
#' @param all A formula with no left hand side/response.
#'
#' Predictors in the formula are included in each subset in the dominance
#' analysis and the R2 value associated with them is subtracted from the
#' overall value.  Predictors in `all` must be present in the model
#' submitted to the `model` argument and cannot be in the `sets` argument.
#'
#' @param quote_args A character vector of arguments in the model submitted to
#' `model` to `quote()` prior to submitting to the dominance analysis.  This
#' is necessary for data masked arguments (e.g., `weights`) to prevent them
#' from being evaluated before being applied to the model and causing an error.
#'
#' @param ...  Not used at current.
#'
#' @return Object of class `"parameters_da"`.
#'
#' An object of class `"parameters_da"` is a list of `data.frame`s composed
#' of the following elements:
#' \describe{
#'  \item{`general`}{A `data.frame` which associates dominance statistics with
#'  model parameters. The variables in this `data.frame` include:
#'   \describe{
#'     \item{`parameter`}{Parameter names.}
#'      \item{`general_dominance`}{Vector of general dominance statistics.}
#'      \item{`standardized`}{Vector of general dominance statistics normalized
#'      to sum to 1.}
#'      \item{`ranks`}{Vector of ranks applied to the general dominance
#'      statistics.}
#'      \item{`subset`}{Names of the subset to which the parameter belongs in
#'      the dominance analysis.  Each other `data.frame` will refer to these
#'      subset names.}}}
#'  \item{`conditional_dominance`}{A `data.frame` of conditional dominance
#'  statistics.  Each observation represents a subset and each variable
#'  represents an the average increment to R2 with a specific number of
#'  subsets in the model.  `NULL` if `conditional` argument is `FALSE`.}
#'  \item{`complete_dominance`}{A `data.frame` of complete dominance
#'  designations. The subsets in the observations are compared to the
#'  subsets referenced in each variable. Whether the subset
#'  in each variable dominates the subset in each observation is
#'  represented in the  logical value. `NULL` if `complete`
#'  argument is `FALSE`..}
#' }
#'
#' @details Computes two decompositions of the model's R2 and returns
#' a matrix of designations from which predictor relative importance
#' determinations can be obtained.
#'
#' Note in the output that the "constant" subset is associated with a
#' component of the model that does not directly contribute to the R2 such
#' as an intercept. The "all" subset is apportioned a component of the fit
#' statistic but is not considered a part of the dominance analysis and
#' therefore does not receive a rank, conditional dominance statistics, or
#' complete dominance designations.
#'
#' The input model is parsed using `insight::find_predictors()`, does not
#' yet support interactions, transformations, or offsets applied in the
#' R formula, and will fail with an error if any such terms are detected.
#'
#' The model submitted must accept an formula object as a `formula`
#' argument.  In addition, the model object must accept the data on which
#' the model is estimated as a `data` argument.  Formulas submitted
#' using object references (i.e., `lm(mtcars$mpg ~ mtcars$vs)`) and
#' functions that accept data as a non-`data` argument
#' (e.g., `survey::svyglm()` uses `design`) will fail with an error.
#'
#' Models that return `TRUE` for the `insight::model_info()`
#' function's values "is_bayesian", "is_mixed", "is_gam",
#' is_multivariate", "is_zero_inflated",
#' or "is_hurdle" are not supported at current.
#'
#' When `performance::r2()` returns multiple values, only the first is used
#' by default.
#'
#' The underlying `domir::domin()` function that implements the dominance
#' statistic and designation computations has only been tested to R version
#' 3.5 and will fail with an error if called in R versions < 3.5.
#'
#' @references
#' - Azen, R., & Budescu, D. V. (2003). The dominance analysis approach
#'   for comparing predictors in multiple regression. Psychological Methods,
#'   8(2), 129-148. doi:10.1037/1082-989X.8.2.129
#'
#' - Budescu, D. V. (1993). Dominance analysis: A new approach to the
#'   problem of relative importance of predictors in multiple regression.
#'   Psychological Bulletin, 114(3), 542-551. doi:10.1037/0033-2909.114.3.542
#'
#' - Groemping, U. (2007). Estimators of relative importance in linear
#'   regression based on variance decomposition. The American Statistician,
#'   61(2), 139-147. doi:10.1198/000313007X188252
#'
#' @seealso [domir::domin()]
#'
#' @author Joseph Luchman
#'
#' @examples
#' if (getRversion() >= "3.5.0" && require("domir")) {
#'   data(mtcars)
#'   model <- glm(vs ~ cyl + carb + mpg, data = mtcars, family = binomial())
#'
#'   r2(model)
#'   dominance_analysis(model)
#'}
#' @export
dominance_analysis <- function(model, sets = NULL, all = NULL,
                               conditional = TRUE, complete = TRUE,
                               quote_args = NULL, ...) {

  # Exit Conditions ----
  insight::check_if_installed("domir")
  insight::check_if_installed("performance")

  if (!insight::is_regression_model(model)) {
    stop(insight::format_message(
      paste(deparse(substitute(model)), "is not a supported insight model."),
      "You may be able to dominance analyze this model using the domir package."
    ), call. = FALSE)
  }

  if (!any(utils::methods(class = class(model)[[1]]) == paste0("r2.", class(model)[[1]]))) {
    stop(insight::format_message(
      paste(deparse(substitute(model)), "does not have a pefromance-supported r2 method."),
      "You may be able to dominance analyze this model using the domir package."
    ), call. = FALSE)
  }

  model_info <- insight::model_info(model)
  if (any(unlist(model_info[c("is_bayesian", "is_mixed", "is_gam", "is_multivariate", "is_zero_inflated", "is_hurdle")]))) {
    stop(insight::format_message(
      paste0("`dominance_analysis()` does not yet support models of class ", class(model), "."),
      "You may be able to dominance analyze this model using the domir package."
    ), call. = FALSE)
  }

  if (!is.null(insight::find_interactions(model))) {
    stop("Interactions in the model formula are not allowed.", call. = FALSE)
  }

  if (!all(insight::find_predictors(model)$conditional %in% attr(stats::terms(insight::find_formula(model)$conditional), "term.labels"))) {
    stop(insight::format_message(
      "Predictors do not match terms.",
      "This usually occurs when there are in-formula predictor transformations such as log(x) or I(x+z)."
    ), call. = FALSE)
  }

  if (!is.null(insight::find_offset(model))) {
    stop("Offsets in the model formula are not allowed.", call. = FALSE)
  }

  if (getRversion() < "3.5") {
    stop("R versions < 3.5 not supported.", call. = FALSE)
  }

  if (!is.null(sets)) {
    if (!is.list(sets)) {
      stop("sets argument must be submitted as list.", call. = FALSE)
    }

    if (length(sets) != length(unlist(sets))) {
      stop("Nested lists are not allowed in sets.", call. = FALSE)
    }

    if (!all(sapply(sets, isa, "formula"))) {
      stop("Each element of list in sets must be a formula.", call. = FALSE)
    }

    if (any(sapply(sets, function(x)  attr(stats::terms(x), "response")==1))) {
      stop("Formulas in sets argument must not have responses/left hand sides.", call. = FALSE)
    }
  }

  if (!is.null(all)) {
    if (!isa(all, "formula")) {
      stop("all argument must be submitted as a formula.", call. = FALSE)
    }

    if (attr(stats::terms(all), "response") == 1) {
      stop("Formula in all argument must not have a response/left hand side.", call. = FALSE)
    }
  }

  if (!is.null(quote_args) && !all(is.character(quote_args))) {
    stop("All arguments in quote_args must be characters.", call. = FALSE)
  }

  # Collect components for arguments ----
  ivs <- insight::find_predictors(model, flatten = TRUE)

  dv <- insight::find_response(model)

  reg <- insight::model_name(model)

  # Process sets ----
  if (!is.null(sets)) {
    # gather predictors from each set
    sets_processed <- lapply(sets, function(x) attr(stats::terms(x), "term.labels"))

    # remove predictors from `ivs` list if in sets
    set_remove_loc <- unlist(lapply(sets_processed, function(x) which(ivs %in% x)))

    if (length(set_remove_loc) != length(unlist(sets_processed))) {
      wrong_set_terms <- unlist(sets_processed)[which(!(unlist(sets_processed) %in% ivs))]

      stop(
        insight::format_message(
          "Terms",
          paste(wrong_set_terms, sep = " "),
          "in sets argument do not match any predictors in model."), call. = FALSE)
    }

    ivs <- ivs[-set_remove_loc]

    # apply names to sets
    set_names <- names(sets)

    missing_set_names <- which(set_names == "")

    if (length(missing_set_names) > 0)
      set_names[missing_set_names] <- paste0("set", missing_set_names)

    if (any(set_names %in% c("all", "constant"))) {
      stop(
        insight::format_message(
          "Names 'all' and 'constant' are reserved for subset names in the dominance_analysis function.",
          "Please rename any sets currently named 'all' or 'constant.'"), call. = FALSE)
    }

    if (any(set_names %in% ivs)) {
      repeat_names <- set_names[which(set_names %in% ivs)]

      stop(insight::format_message(
        "Set names",
        paste(repeat_names, sep = " "), "are also the names of invidiual predictors.",
           "Please rename these sets."), call. = FALSE)
    }

  }

  else sets_processed <- NULL

  # Process all ----
  if (!is.null(all)) {
    # gather predictors in all
    all_processed <- attr(stats::terms(all), "term.labels")

    # remove predictors in all from `ivs` list
    all_remove_loc <-  which(ivs %in% all_processed)

    if (any(all_processed %in% unlist(sets_processed))) {
      reused_terms <- all_processed[which(all_processed %in% unlist(sets_processed))]

      stop(insight::format_message(
        "Terms",
        paste(reused_terms, sep = " "),
        "in all argument are also used in sets argument."), call. = FALSE)
    }

    if (length(all_remove_loc) != length(unlist(all_processed))) {
      wrong_all_terms <- all_processed[which(!(all_processed) %in% ivs)]

      stop(insight::format_message(
        "Terms",
        paste(wrong_all_terms, sep = " "),
        "in all argument do not match any predictors in model."), call. = FALSE)
    }

    ivs <- ivs[-all_remove_loc] # update IVs

  }

  else all_processed <- NULL

  # name collisions across subsets - exit
  if (any(ivs %in% c("all", "constant"))) {
    stop(
      insight::format_message(
        "Names 'all' and 'constant' are reserved for subset names in the dominance_analysis function.",
        "Please rename any predictors currently named 'all' or 'constant.'",
        "Alternatively, put the predictor in a set by itself."), call. = FALSE)
  }

  # big DA warning
  if (length(c(ivs, unlist(sets_processed))) > 15) warning(
    cat(paste("Total of", 2^length(ivs)-1, "models to be estimated.\n",
              "Process may take some time.")) , call. = FALSE)

  # Build non-formula model arguments to `domin` ----
  fml <- stats::reformulate(ivs, response = dv, intercept = insight::has_intercept(model))

  data <- insight::get_data(model)

  args <- as.list(insight::get_call(model), collapse = "") # extract all arguments from call

  loc <- which(!(names(args) %in% c("formula", "data"))) # find formula and data arguments

  if (length(which(names(args) %in% c("formula", "data"))) != 2) { # exit if formula and data arguments missing
    stop("Model submitted does not have a formula and data argument.", call. = FALSE)
  }

  args <- args[loc] # remove formula and data arguments
  args <- args[-1] # remove function name

  # quote arguments for domin
  for (arg in quote_args) {
    if (!(arg %in% names(args))) stop(arg, " in quote_args not among arguments in model.", call. = FALSE)

    else args[[arg]] <- str2lang(paste0("quote(", deparse(args[[arg]]), ")", collapse = ""))

  }

  # Internal wrapper to ensure r2 values conform to domin ----
  r2_wrap <- function(model, ...) {
    list(fitstat = performance::r2(model, ...)[[1]])
  }

  # Finalize and implement DA
  args2domin <- append(list(formula_overall = fml, reg = reg, fitstat = list(r2_wrap, "fitstat"),
                    data = data, conditional = conditional, complete = complete,
                    sets = sets_processed, all = all_processed), args)

  utils::capture.output(domir_res <- do.call(domir::domin, args2domin))

  # Set up returned data.frames ----
  if (!is.null(sets)) {

    names(domir_res$General_Dominance) <-
      c(names(domir_res$General_Dominance)[1:(length(domir_res$General_Dominance) - length(set_names))],
        set_names)

    if (conditional)
      rownames(domir_res$Conditional_Dominance) <- names(domir_res$General_Dominance)

  }

  if (complete) {

    colnames(domir_res$Complete_Dominance) <- paste0("< ", names(domir_res$General_Dominance))

    dimnames(domir_res$Complete_Dominance) <- list(
      colnames(domir_res$Complete_Dominance),
      names(domir_res$General_Dominance)
    )

    domir_res$Complete_Dominance <- t(domir_res$Complete_Dominance)

  }

  da_df_res <- da_df_cat <-
    data.frame(parameter = insight::find_parameters(model, flatten = TRUE))

  da_df_cat = data.frame(da_df_cat, subset = NA_character_)

  if (!is.null(sets)) {

    for (set in 1:length(sets)) {

      set_name <- ifelse(!is.null(names(sets)[[set]]), names(sets)[[set]],
                         paste0("set", set))

      da_df_cat$subset <-
        replace(da_df_cat$subset,
                da_df_res$parameter %in% all.vars(sets[[set]]), set_name)

    }

  }

  if (!is.null(all)) {

      da_df_cat$subset <-
        replace(da_df_cat$subset,
                da_df_res$parameter %in% all.vars(all), "all")

  }

  da_df_cat$subset <-
    ifelse((da_df_res$parameter %in% (insight::find_predictors(model, flatten = TRUE))) &
             (is.na(da_df_cat$subset)),
           da_df_res$parameter,
           da_df_cat$subset)

  da_df_cat$subset <-
    replace(da_df_cat$subset,
            is.na(da_df_cat$subset), "constant")

  da_df_res <-
    datawizard::data_merge(da_df_cat,
                           data.frame(subset = names(domir_res$General_Dominance),
                                      general_dominance = domir_res$General_Dominance))

  if (!is.null(all)) {

    da_df_res$general_dominance <-
      replace(da_df_res$general_dominance, da_df_res$subset == "all", domir_res$Fit_Statistic_All_Subsets)

  }

  da_df_res <-
    datawizard::data_merge(da_df_res,
                           data.frame(subset = names(domir_res$General_Dominance),
                                      standardized = domir_res$Standardized))

  da_df_res <-
    datawizard::data_merge(da_df_res,
                           data.frame(subset = names(domir_res$General_Dominance),
                                      ranks = domir_res$Ranks))

  da_df_res <-
    datawizard::data_relocate(da_df_res, "subset", after = "ranks")

  if (conditional) {

    da_df_cdl <-
      data.frame(subset = names(domir_res$General_Dominance))

    da_df_cdl <-
      datawizard::data_merge(da_df_cdl,
                             data.frame(subset = names(domir_res$General_Dominance),
                                        domir_res$Conditional_Dominance))

    da_df_cdl <-
      datawizard::data_rename(da_df_cdl,
                              names(da_df_cdl)[2:length(da_df_cdl)],
                              colnames(domir_res$Conditional_Dominance))
  }

  else da_df_cdl <- NULL

  if (complete) {

  da_df_cpt <-
    data.frame(subset = names(domir_res$General_Dominance))

  da_df_cpt <-
    datawizard::data_merge(da_df_cpt,
                           data.frame(subset = names(domir_res$General_Dominance),
                                      domir_res$Complete_Dominance))

  da_df_cpt <-
    datawizard::data_rename(da_df_cpt,
                            names(da_df_cpt)[2:length(da_df_cpt)],
                            colnames(domir_res$Complete_Dominance))

  }

  else da_df_cpt <- NULL

  da_list <- list(general = da_df_res,
                  conditional = da_df_cdl,
                  complete = da_df_cpt)

  # add attributes and class
  attr(da_list, "model_R2") <- domir_res$Fit_Statistic_Overall
  attr(da_list$general, "table_title") <- "General Dominance Statistics"
  if (conditional) attr(da_list$conditional, "table_title") <- "Conditional Dominance Statistics"
  if (complete) attr(da_list$complete, "table_title") <- "Complete Dominance Designations"

  class(da_list) <- c("parameters_da")

  da_list

}




# methods ------------------------------


#' @export
print.parameters_da <- function(x, digits = 3, ...) {
  insight::print_color("# Dominance Analysis Results", "blue")
  cat("\n\n")

  cat("Model R2 Value: ", sprintf("%.*f", digits, attr(x, "model_R2")), "\n\n")

  cat(insight::export_table(x, digits = digits, ...))

  invisible(x)

}
