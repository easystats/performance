#' @title Compute second order AIC
#' @name performance_aicc
#'
#' @description Compute the second-order Akaike's information criterion (AICc).
#' The second-order (or small sample) is a AIC with a correction for small sample
#' sizes.
#'
#' @param x A model object.
#' @param ... Currently not used.
#'
#' @return Numeric, the AICc value.
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
#'
#' @export
performance_aicc <- function(x, ...) {
  UseMethod("performance_aicc")
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
