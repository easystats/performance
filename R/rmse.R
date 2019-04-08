#' @title Root Mean Squared Error of Linear Models
#' @name rmse
#'
#' @description Compute root mean squared error for linear (mixed effects) models.
#'
#' @param model Linear model of class \code{lm}, \code{merMod} (\pkg{lme4})
#'   or \code{lme} (\pkg{nlme}).
#' @param normalized Logical, use \code{TRUE} if normalized rmse should be returned.
#'
#' @details The RMSE is the square root of the variance of the residuals and indicates
#'   the absolute fit of the model to the data (difference between observed data
#'   to model's predicted values). It can be interpreted as the standard
#'   deviation of the unexplained variance, and is in the same units as the
#'   response variable. Lower values indicate better model fit.
#'   \cr \cr
#'   The normalized RMSE is the proportion of the RMSE related to the
#'   range of the response variable. Hence, lower values indicate
#'   less residual variance.
#'
#' @return Numeric, the root mean squared error.
#'
#' @examples
#' library(nlme)
#' m <- lme(distance ~ age, data = Orthodont)
#'
#' # RMSE
#' rmse(m, normalized = TRUE)
#'
#' # normalized RMSE
#' rmse(m, normalized = TRUE)
#'
#' @importFrom insight get_response
#' @export
rmse <- function(model, normalized = FALSE) {
  # compute rmse
  rmse_val <- sqrt(mse(model))

  # if normalized, divide by range of response
  if (normalized) {
    # get response
    resp <- insight::get_response(model)
    # cpmpute rmse, normalized
    rmse_val <- rmse_val / (max(resp, na.rm = T) - min(resp, na.rm = T))
  }

  rmse_val
}
