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
#' @family checking model assumptions and quality
#'
#' @details Performs a Durbin-Watson-Test to check for autocorrelated residuals.
#' In case of autocorrelation, robust standard errors return more accurate
#' results for the estimates, or maybe a mixed model with error term for the
#' cluster groups should be used.
#'
#' @examples
#' m <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
#' check_autocorrelation(m)
#' @export
check_autocorrelation <- function(x, ...) {
  UseMethod("check_autocorrelation")
}

#' @rdname check_autocorrelation
#' @export
check_autocorrelation.default <- function(x, nsim = 1000, ...) {
  # check for valid input
  .is_model_valid(x)

  .residuals <- stats::residuals(x)
  n <- length(.residuals)
  dw <- .durbWats(.residuals)

  X <- insight::get_modelmatrix(x)
  mu <- stats::fitted(x)
  Y <- matrix(sample(.residuals, n * nsim, replace = TRUE), n, nsim) + matrix(mu, n, nsim)
  E <- stats::residuals(stats::lm(Y ~ X - 1))
  DW <- rbind(apply(E, 2, .durbWats))

  p <- (sum(dw < DW[1, ])) / nsim
  p.val <- 2 * (min(p, 1 - p))

  class(p.val) <- c("check_autocorrelation", "see_check_autocorrelation", class(p.val))
  p.val
}



# methods ------------------------------

#' @export
plot.check_autocorrelation <- function(x, ...) {
  insight::format_warning("There is currently no `plot()` method for `check_autocorrelation()`.")
}


#' @export
print.check_autocorrelation <- function(x, ...) {
  if (x < 0.05) {
    insight::print_color(
      sprintf(
        "Warning: Autocorrelated residuals detected (%s).",
        insight::format_p(x)
      ),
      "red"
    )
  } else {
    insight::print_color(
      sprintf(
        "OK: Residuals appear to be independent and not autocorrelated (%s).",
        insight::format_p(x)
      ),
      "green"
    )
  }
  invisible(x)
}



# utilities -------------------------------

.durbWats <- function(.residuals) {
  n <- length(.residuals)
  den <- sum(.residuals^2)
  (sum((.residuals[2:n] - .residuals[1:(n - 1)])^2)) / den
}
