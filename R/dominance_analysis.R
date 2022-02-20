#' @title Dominance Analysis
#' @name dominance_analysis
#'
#' @description Computes Dominance Statistics and Designations
#'
#' @param model An \code{insight}- and \code{performance::r2}-supported model.
#' @param ... Arguments passed to \code{domir::domin} function.
#'
#' @return Object of class \code{dominance_analysis}.
#'
#' @details Dominance analysis discussion here...
#'
#' At current, the model submitted must accept a formula as a \code{formula}
#' argument and must accept the data on which the model is estimated as a
#' \code{data} argument.
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
#' @examples
#'
#'   model <- glm(vs ~ cyl + carb + mpg, data = datasets::mtcars, family = binomial())
#'
#'   r2(model)
#'
#'   dominance_analysis(model)
#'
#' @export

dominance_analysis <- function(model, ...) {

  # Exit Conditions
  if (!requireNamespace("domir", quietly = TRUE)) stop("package domir is required for use of performance::dominance_analysis()).")

  if (!insight::is_regression_model(model)) stop(paste(deparse(substitute(model)), "is not a supported insight model.\nYou may be able to dominance analyze this model using the domir package."))

  if (!any(methods(class = class(model)[[1]])==paste0("r2.", class(model)[[1]]))) stop(paste(deparse(substitute(model)), "does not have a pefromance-supported r2 method.\nYou may be able to dominance analyze this model using the domir package."))

  model_info <- insight::model_info(model)
  if (any(unlist(model_info[c("is_bayesian", "is_mixed", "is_gam", "is_multivariate", "is_zero_inflated", "is_hurdle")])))
    stop(paste("dominance_analysis does not support models of", class(model), "yet.\nYou may be able to dominance analyze this model using the domir package."))


  # Collect components for arguments
  ivs <- insight::find_predictors(model, flatten = TRUE)
  dv <- insight::find_response(model)
  reg <- insight::model_name(model)
  fml <- stats::reformulate(ivs, response = dv)
  data <- insight::get_data(model)
  args <- as.list(str2lang(deparse(insight::get_call(model)))) # extract all arguments from call
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
  capture.output(da_res <- do.call(domir::domin, args2domin))

  class(da_res) <- c("dominance_analysis", "domin", "list")

  return(da_res)

}

print.dominance_analysis <- function(x, ...) {

  #tbd

}
