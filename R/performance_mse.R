#' @title Mean Square Error of Linear Models
#' @name performance_mse
#'
#' @description Compute mean square error of linear models.
#'
#' @inheritParams performance_rmse
#' @inheritParams model_performance.lm
#'
#' @details The mean square error is the mean of the sum of squared residuals,
#'   i.e. it measures the average of the squares of the errors. Less technically
#'   speaking, the mean square error can be considered as the variance of the
#'   residuals, i.e. the variation in the outcome the model doesn't explain.
#'   Lower values (closer to zero) indicate better fit.
#'
#' @return Numeric, the mean square error of \code{model}.
#'
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ hp + gear, data = mtcars)
#' performance_mse(m)
#' @importFrom insight get_residuals
#' @export
performance_mse <- function(model, ...) {
  UseMethod("performance_mse")
}

#' @rdname performance_mse
#' @export
mse <- performance_mse



#' @export
performance_mse.default <- function(model, verbose = TRUE, ...) {
  res <- tryCatch(
    {
      insight::get_residuals(model, verbose = verbose, type = "response", ...)
    },
    error = function(e) {
      NULL
    }
  )

  if (is.null(res)) {
    res <- tryCatch(
      {
        def_res <- insight::get_residuals(model, verbose = verbose, ...)
        if (verbose) {
          warning("Response residuals not available to calculate mean square error. (R)MSE is probably not reliable.", call. = FALSE)
        }
        def_res
      },
      error = function(e) {
        NULL
      }
    )
  }

  if (is.null(res) || all(is.na(res))) {
    return(NA)
  }

  # for multivariate response models...
  if (is.data.frame(res)) {
    if (verbose) {
      warning("Multiple response variables detected. Cannot reliably compute (R)MSE.", call. = FALSE)
    }
    return(NA)
  }

  mean(res^2, na.rm = TRUE)
}




# mfx models -------------------------------

#' @export
performance_mse.logitor <- function(model, verbose = TRUE, ...) {
  performance_mse(model$fit, verbose = verbose, ...)
}

#' @export
performance_mse.logitmfx <- performance_mse.logitor

#' @export
performance_mse.probitmfx <- performance_mse.logitor

#' @export
performance_mse.poissonirr <- performance_mse.logitor

#' @export
performance_mse.poissonmfx <- performance_mse.logitor

#' @export
performance_mse.negbinirr <- performance_mse.logitor

#' @export
performance_mse.negbinmfx <- performance_mse.logitor

#' @export
performance_mse.betaor <- performance_mse.logitor

#' @export
performance_mse.betamfx <- performance_mse.logitor

#' @export
performance_mse.model_fit <- performance_mse.logitor
