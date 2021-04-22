#' @title Root Mean Squared Error
#' @name performance_rmse
#'
#' @description Compute root mean squared error for (mixed effects) models,
#'   including Bayesian regression models.
#'
#' @param model A model.
#' @param normalized Logical, use \code{TRUE} if normalized rmse should be returned.
#' @inheritParams model_performance.lm
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
#' if (require("nlme")) {
#'   m <- lme(distance ~ age, data = Orthodont)
#'
#'   # RMSE
#'   performance_rmse(m, normalized = FALSE)
#'
#'   # normalized RMSE
#'   performance_rmse(m, normalized = TRUE)
#' }
#' @export
performance_rmse <- function(model, normalized = FALSE, verbose = TRUE) {
  tryCatch(
    {
      # compute rmse
      rmse_val <- sqrt(performance_mse(model, verbose = verbose))

      # if normalized, divide by range of response
      if (normalized) {
        # get response
        resp <- .factor_to_numeric(insight::get_response(model, verbose = verbose))
        # compute rmse, normalized
        rmse_val <- rmse_val / (max(resp, na.rm = TRUE) - min(resp, na.rm = TRUE))
      }

      rmse_val
    },
    error = function(e) {
      if (inherits(e, c("simpleError", "error")) && verbose) {
        insight::print_color(e$message, "red")
        cat("\n")
      }
      NA
    }
  )
}


#' @rdname performance_rmse
#' @export
rmse <- performance_rmse
