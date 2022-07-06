#' @title Mean Absolute Error of Models
#' @name performance_mae
#'
#' @description Compute mean absolute error of models.
#'
#' @inheritParams performance_rmse
#' @inheritParams model_performance.lm
#'
#' @return Numeric, the mean absolute error of `model`.
#'
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ hp + gear, data = mtcars)
#' performance_mae(m)
#' @export
performance_mae <- function(model, ...) {
  UseMethod("performance_mae")
}

#' @rdname performance_mae
#' @export
mae <- performance_mae



#' @export
performance_mae.default <- function(model, verbose = TRUE, ...) {
  pred <- tryCatch(
    {
      insight::get_predicted(model, ci = NULL, verbose = verbose, ...)
    },
    error = function(e) {
      NULL
    }
  )

  observed <- insight::get_response(model)
  mean(abs(observed - pred))
}




# mfx models -------------------------------

#' @export
performance_mae.logitor <- function(model, verbose = TRUE, ...) {
  performance_mae(model$fit, verbose = verbose, ...)
}

#' @export
performance_mae.logitmfx <- performance_mae.logitor

#' @export
performance_mae.probitmfx <- performance_mae.logitor

#' @export
performance_mae.poissonirr <- performance_mae.logitor

#' @export
performance_mae.poissonmfx <- performance_mae.logitor

#' @export
performance_mae.negbinirr <- performance_mae.logitor

#' @export
performance_mae.negbinmfx <- performance_mae.logitor

#' @export
performance_mae.betaor <- performance_mae.logitor

#' @export
performance_mae.betamfx <- performance_mae.logitor

#' @export
performance_mae.model_fit <- performance_mae.logitor
