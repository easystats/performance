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
#' @param ...  Not used at current.
#'
#' @return Object of class `"dominance_analysis"`.
#'
#' An object of class `"dominance_analysis"` is a list composed of the
#' following elements:
#' \describe{
#'  \item{`general_dominance`}{Vector of general dominance statistics.}
#'  \item{`standardized`}{Vector of general dominance statistics normalized
#'  to sum to 1.}
#'  \item{`ranks`}{Vector of ranks applied to the general dominance
#'  statistics.}
#'  \item{`conditional_dominance`}{Matrix of conditional dominance
#'  statistics.  Each row represents a predictor; each column represents an
#'  the average increment to R2 with a specific number of predictors in
#'  the model or `NULL`.}
#'  \item{`complete_dominance`}{Logical matrix of complete dominance
#'  designations. The predictors represented in each row are
#'  cross-referenced with predictors in each column.  Whether the predictor
#'  in each column dominates the predictor in each row is represented in the
#'  logical value in the matrix or `NULL`.}
#'  \item{`model_R2`}{Value of R2 value returned by the `r2()` method for
#'  the model.}
#'  \item{`all_subset_R2`}{Value of R2 associated with the predictors in the
#'  `all` argument or `NULL`.}
#' }
#'
#' @details Computes two decompositions of the model's R2 and returns
#' a matrix of designations from which predictor relative importance
#' determinations can be obtained.
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
#' (e.g., `survey::svyglm()` uses `design`) will fail.
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
#' - Luchman, J. N., Lei, X., & Kaplan, S. A. (2020). Relative
#'   Importance Analysis With Multivariate Models: Shifting the Focus from
#'   Independent Variables to Parameter Estimates. Journal of Applied
#'   Structural Equation Modeling, 4(2), 1-20. doi:10.47263/JASEM.4(2)02
#'
#' - Luchman, J. N. (2021). Determining relative importance in Stata
#'   using dominance analysis: domin and domme. Stata Journal 21(2),
#'   510-538. doi:10.1177/1536867X211025837
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
                               conditional = TRUE, complete = TRUE, ...) {

  # Exit Conditions
  insight::check_if_installed("domir")

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

  # Collect components for arguments
  ivs <- insight::find_predictors(model, flatten = TRUE)
  dv <- insight::find_response(model)
  reg <- insight::model_name(model)

  # Process sets
  if (!is.null(sets)) {
    sets_processed <- lapply(sets, function(x) attr(stats::terms(x), "term.labels"))

    set_remove_loc <- unlist(lapply(sets_processed, function(x) which(ivs %in% x)))

    if (length(set_remove_loc) != length(unlist(sets_processed))) {
      wrong_set_terms <- unlist(sets_processed)[which(!(unlist(sets_processed) %in% ivs))]

      stop(
        insight::format_message(
          "Terms",
          paste(wrong_set_terms, sep = " "),
          "in sets argument do not match any predictors in model."), call. = FALSE)
    }

    ivs <- ivs[-set_remove_loc] # update IVs

  }

  else sets_processed <- NULL

  # Process all
  if (!is.null(all)) {
    all_processed <- attr(stats::terms(all), "term.labels")

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

  fml <- stats::reformulate(ivs, response = dv, intercept = insight::has_intercept(model))

  data <- insight::get_data(model)
  args <- as.list(insight::get_call(model), collapse = "") # extract all arguments from call
  loc <- which(!(names(args) %in% c("formula", "data"))) # find formula and data arguments <- must ensure these are always there - will not for {survey} -- todo: is there better way to capture "extra" arguments?
  args <- args[loc] # remove formula and data arguments
  args <- args[-1] # remove function name
  r2_wrap <- function(model, ...) { # internal wrapper to ensure r2 values conform to domin
    list(fitstat = performance::r2(model, ...)[[1]]) # todo: must deal with multiple returned values
  }

  args2domin <- append(list(formula_overall = fml, reg = reg, fitstat = list(r2_wrap, "fitstat"),
                    data = data, conditional = conditional, complete = complete,
                    sets = sets_processed, all = all_processed), args)

  # Implement DA
  utils::capture.output(da_res <- do.call(domir::domin, args2domin))

  da_res <-
    da_res[which(names(da_res) %in%
                   c("Fit_Statistic_Overall", "Fit_Statistic_All_Subsets",
                     "General_Dominance", "Conditional_Dominance", "Complete_Dominance",
                     "Standardized", "Ranks"))]
  names(da_res) <- c("general_dominance", "standardized", "ranks",
                     "conditional_dominance", "complete_dominance",
                     "model_R2", "all_subset_R2")

  if (!is.null(sets)) {
    if (!is.null(names(sets))) {
      set_names <- names(sets)
      missing_set_names <- which(set_names == "")
      if (length(missing_set_names) > 0)
        set_names[missing_set_names] <- paste0("set", missing_set_names)

      names(da_res$general_dominance) <-
        c(names(da_res$general_dominance)[1:(length(da_res$general_dominance) - length(set_names))],
          set_names)

      if (!is.null(da_res$conditional_dominance))
        rownames(da_res$conditional_dominance) <- names(da_res$general_dominance)

      if (!is.null(da_res$complete_dominance))
        colnames(da_res$complete_dominance) <- paste0("Dmnated_", names(da_res$general_dominance))

    }
  }

  if (complete) {
    dimnames(da_res$complete_dominance) <- list(
      colnames(da_res$complete_dominance),
      names(da_res$general_dominance)
    )

    da_res$complete_dominance <- t(da_res$complete_dominance)

  }

  if (is.null(all)) da_res$all_subset_R2 <- NULL



  class(da_res) <- c("dominance_analysis", "domin", "list")
  da_res
}




# methods ------------------------------


#' @export
print.dominance_analysis <- function(x, digits = 3, ...) {
  insight::print_color("# Dominance Analysis Results", "blue")
  cat("\n\n")

  cat("Model R2 Value:                 ", sprintf("%.*f", digits, x$model_R2), "\n")

  if (!is.null(x$all_subset_R2))
    cat("All Subsets Predictors R2 Value:", sprintf("%.*f", digits, x$all_subset_R2), "\n\n")
  else cat("\n")

  cat("General Dominance Statistics\n")

  Display_gnr <-
    data.frame(`General Dominance` = x[["general_dominance"]],
               `Standardized` = x[["standardized"]],
               `Ranks` = as.character(x[["ranks"]]), check.names = FALSE,
               row.names = names(x[["general_dominance"]]),
               stringsAsFactors = FALSE)

  print(insight::format_table(Display_gnr,
                              digits = digits))

  if (!is.null(x$conditional_dominance)) {
    Display_cdl <- as.data.frame(x$conditional_dominance)

    names(Display_cdl) <- gsub("IVs_", "Preds: ", names(Display_cdl))

    cat("\nConditional Dominance Statistics\n")

    print(insight::format_table(Display_cdl,
                                digits = digits))

  }

  if (!is.null(x$complete_dominance)) {
    Display_cpt <- as.data.frame(x$complete_dominance)

    colnames(Display_cpt) <- gsub("Dmnated_", "Dominated by: ", colnames(Display_cpt))

    cat("\nComplete Dominance Designations\n")

    print(insight::format_table(Display_cpt))

  }

  invisible(x)
}
