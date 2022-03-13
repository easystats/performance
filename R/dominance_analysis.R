#' @title Dominance Analysis
#' @name dominance_analysis
#'
#' @description Computes Dominance Analysis Statistics and Designations
#'
#' @param model An `{insight}`- and `performance::r2`-supported model object
#' of specific classes.  See Notes section.
#' @param ...  Not used at current.
#'
#' @return Object of class "dominance_analysis".
#'
#' An object of class "dominance_analysis" is a list composed of the
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
#'  the model.}
#'  \item{`complete_dominance`}{Logical matrix of complete dominance
#'  designations. The predictors represented in each row are
#'  cross-referenced with predictors in each column.  Whether the predictor
#'  in each column dominates the predictor in each row is represented in the
#'  logical value in the matrix.}
#'  \item{`model_R2`}{Value of R2 value returned by the `r2`
#'  method for the model.}
#' }
#'
#' @details Computes two decompositions of the model's R2 and returns
#' a matrix of designations from which predictor relative importance
#' determinations can be obtained.
#'
#' @section Notes:
#'
#' The input model is parsed using `insight::find_predictors` and does not
#' yet support interactions, transformations, or offsets applied in the
#' R formula and will fail with an error when detected.
#'
#' The model submitted must accept an formula object as a `formula`
#' argument.  In addition, the model object must accept the data on which
#' the model is estimated as a `data` argument.  Formulas submitted
#' using object references (i.e., `lm(mtcars$mpg ~ mtcars$vs)`) and
#' functions that accept data as a non-`data` argument
#' (e.g., `survey::svyglm` uses `design`) will fail.
#'
#' Models that return \code{TRUE} for the \code{insight::model_info}
#' function's values "is_bayesian", "is_mixed", "is_gam",
#' is_multivariate", "is_zero_inflated",
#' or "is_hurdle" are not supported at current.
#'
#' When `performance::r2` returns multiple values, only the first is used
#' by default.
#'
#' @references
#' \itemize{
#'   \item Azen, R., & Budescu, D. V. (2003). The dominance analysis approach
#'   for comparing predictors in multiple regression. Psychological Methods,
#'   8(2), 129-148. doi:10.1037/1082-989X.8.2.129
#'
#'   \item Budescu, D. V. (1993). Dominance analysis: A new approach to the
#'   problem of relative importance of predictors in multiple regression.
#'   Psychological Bulletin, 114(3), 542-551. doi:10.1037/0033-2909.114.3.542
#'
#'   \item Groemping, U. (2007). Estimators of relative importance in linear
#'   regression based on variance decomposition. The American Statistician,
#'   61(2), 139-147. doi:10.1198/000313007X188252
#'
#'   \item Luchman, J. N., Lei, X., & Kaplan, S. A. (2020). Relative
#'   Importance Analysis With Multivariate Models: Shifting the Focus from
#'   Independent Variables to Parameter Estimates. Journal of Applied
#'   Structural Equation Modeling, 4(2), 1-20. doi:10.47263/JASEM.4(2)02
#'
#'   \item Luchman, J. N. (2021). Determining relative importance in Stata
#'   using dominance analysis: domin and domme. Stata Journal 21(2),
#'   510-538. doi:10.1177/1536867X211025837
#' }
#'
#' @seealso [domir::domin()]
#'
#' @author Joseph Luchman
#'
#' @examples
#' if (require("domir")) {
#'   model <- glm(vs ~ cyl + carb + mpg, data = datasets::mtcars,
#'            family = binomial())
#'
#'   r2(model)
#'
#'   dominance_analysis(model)
#'
#'}
#'
#' @export

dominance_analysis <- function(model, ...) {

  # Exit Conditions
  if (!requireNamespace("domir", quietly = TRUE)) stop("package domir is required.")

  if (!insight::is_regression_model(model)) stop(paste(deparse(substitute(model)), "is not a supported insight model.\nYou may be able to dominance analyze this model using the domir package."))

  if (!any(utils::methods(class = class(model)[[1]])==paste0("r2.", class(model)[[1]]))) stop(paste(deparse(substitute(model)), "does not have a pefromance-supported r2 method.\nYou may be able to dominance analyze this model using the domir package."))

  model_info <- insight::model_info(model)
  if (any(unlist(model_info[c("is_bayesian", "is_mixed", "is_gam", "is_multivariate", "is_zero_inflated", "is_hurdle")])))
    stop(paste0("dominance_analysis does not yet support models of class ", class(model), ".\nYou may be able to dominance analyze this model using the domir package."))

  if (!is.null(insight::find_interactions(model))) stop("interactions in the model formula are not allowed.")

  if (!all(insight::find_predictors(model)$conditional %in% attr(stats::terms(insight::find_formula(model)$conditional), "term.labels"))) stop("predictors do not match terms.\nThis usually occurs when there are in-formula predictor transformations such as log(x) or I(x+z).")

  if (!is.null(insight::find_offset(model))) stop("offsets in the model formula are not allowed.")

  # Collect components for arguments
  ivs <- insight::find_predictors(model, flatten = TRUE)
  dv <- insight::find_response(model)
  reg <- insight::model_name(model)
  fml <- stats::reformulate(ivs, response = dv, intercept = insight::has_intercept(model))
  data <- insight::get_data(model)
  args <- as.list(insight::get_call(model), collapse = "") # extract all arguments from call
  loc <- which(!(names(args) %in% c("formula", "data"))) # find formula and data arguments <- must ensure these are always there - will not for {survey} -- todo: is there better way to capture "extra" arguments?
  args <- args[loc] # remove formula and data arguments
  args <- args[-1] # remove function name
  r2_wrap <- function(model, ...) { # internal wrapper to ensure r2 values conform to domin
    fitstat <- list(fitstat = performance::r2(model, ...)[[1]]) # todo: must deal with multiple returned values
    return(fitstat)
  }

  args2domin <- append(list(formula_overall = fml, reg = reg, fitstat = list(r2_wrap, "fitstat"),
                    data = data), args)

  # Implement DA
  utils::capture.output(da_res <- do.call(domir::domin, args2domin))

  da_res <- da_res[which(names(da_res) %in% c("Fit_Statistic_Overall", "General_Dominance", "Conditional_Dominance", "Complete_Dominance", "Standardized", "Ranks"))]

  names(da_res)<- c("general_dominance", "standardized", "ranks", "conditional_dominance", "complete_dominance", "model_R2")

  dimnames(da_res$complete_dominance) <- list(colnames(da_res$complete_dominance), names(da_res$general_dominance))

  da_res$complete_dominance <- t(da_res$complete_dominance)

  class(da_res) <- c("dominance_analysis", "domin", "list")

  return(da_res)

}
