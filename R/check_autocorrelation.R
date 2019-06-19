#' @title Check model for independence of residuals.
#' @name check_autocorrelation
#'
#' @description Check model for independence of residuals, i.e. for autocorrelation
#' of error terms.
#'
#' @param x A model object.
#' @param nsim Number of simulations for the Durbin-Watson-Test.
#' @param ... Currently not used.
#'
#' @return Invisibly returns the p-value of the test statistics. A p-value < 0.05
#' indicates autocorrelated residuals.
#'
#' @details Performs a Durbin-Watson-Test to check for autocorrelated residuals.
#' In case of autocorrelation, robust standard errors return more accurate
#' results for the estimates, or maybe a mixed model with error term for the
#' cluster groups should be used.
#'
#' @examples
#' m <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
#' check_autocorrelation(m)
#'
#' @importFrom stats residuals model.matrix fitted
#' @export
check_autocorrelation <- function(x, ...) {
  UseMethod("check_autocorrelation")
}

#' @rdname check_autocorrelation
#' @export
check_autocorrelation.default <- function(x, nsim = 1000, ...) {
  .residuals <- stats::residuals(x)
  n <- length(.residuals)
  dw <- .durbWats(.residuals)

  X <- stats::model.matrix(x)
  mu <- stats::fitted(x)
  Y <- matrix(sample(.residuals, n * nsim, replace = TRUE), n, nsim) + matrix(mu, n, nsim)
  E <- stats::residuals(lm(Y ~ X - 1))
  DW <- rbind(apply(E, 2, .durbWats))

  p <- (sum(dw < DW[1, ])) / nsim
  p.val <- 2 * (min(p, 1 - p))

  if (p.val < 0.05) {
    insight::print_color(sprintf("Warning: Autocorrelated residuals detected (p = %.3f).", p.val), "red")
  } else {
    insight::print_color(sprintf("OK: Residuals appear to be independent and not autocorrelated (p = %.3f).", p.val), "green")
  }

  invisible(p.val)
}

.durbWats <- function(.residuals) {
  n <- length(.residuals)
  den <- sum(.residuals ^ 2)
  (sum((.residuals[2:n] - .residuals[1:(n - 1)]) ^ 2)) / den
}
