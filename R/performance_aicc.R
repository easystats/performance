#' @title Compute AIC and second order AIC
#' @name performance_aicc
#'
#' @description Compute the second-order Akaike's information criterion (AICc).
#' The second-order (or small sample) is a AIC with a correction for small sample
#' sizes. \code{performance_aic()} is a small wrapper that returns the AIC. It is
#' a generic function that also works for some models that don't have a AIC method
#' (like Tweedie models).
#'
#' @param x A model object.
#' @param ... Currently not used.
#'
#' @return Numeric, the AIC or AICc value.
#'
#' @references \itemize{
#' \item Akaike, H. (1973) Information theory as an extension of the maximum likelihood principle. In: Second International Symposium on Information Theory, pp. 267–281. Petrov, B.N., Csaki, F., Eds, Akademiai Kiado, Budapest.
#' \item Hurvich, C. M., Tsai, C.-L. (1991) Bias of the corrected AIC criterion for underfitted regression and time series models. Biometrika 78, 499–509.
#' }
#'
#' @examples
#' m <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
#' AIC(m)
#' performance_aicc(m)
#' @export
performance_aicc <- function(x, ...) {
  UseMethod("performance_aicc")
}

#' @rdname performance_aicc
#' @importFrom stats AIC
#' @export
performance_aic <- function(x, ...) {
  if (inherits(x, c("vgam", "vglm"))) {
    if (!requireNamespace("VGAM", quietly = TRUE)) {
      warning("Package 'VGAM' required for this function work. Please install it.", call. = FALSE)
      return(NULL)
    }
    VGAM::AIC(x)
  } else if (insight::model_info(x)$family == "Tweedie") {
    if (!requireNamespace("tweedie", quietly = TRUE)) {
      warning("Package 'tweedie' required for this function work. Please install it.", call. = FALSE)
      return(NULL)
    }
    suppressMessages(tweedie::AICtweedie(x))
  } else {
    tryCatch(
      {
        stats::AIC(x)
      },
      error = function(e) {
        NULL
      }
    )
  }
}


#' @importFrom insight n_obs
#' @importFrom stats logLik
#' @export
performance_aicc.default <- function(x, ...) {
  n <- insight::n_obs(x)
  ll <- stats::logLik(x)
  k <- attr(ll, "df")

  -2 * as.vector(ll) + 2 * k * (n / (n - k - 1))
}


#' @export
performance_aicc.vglm <- function(x, ...) {
  if (!requireNamespace("VGAM", quietly = TRUE)) {
    warning("Package 'VGAM' required for this function work. Please install it.", call. = FALSE)
    return(NULL)
  }
  VGAM::AICc(x)
}


#' @export
performance_aicc.rma <- function(x, ...) {
  stats::AIC(x, correct = TRUE)
}
