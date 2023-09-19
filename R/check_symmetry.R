#' Check distribution symmetry
#'
#' Uses Hotelling and Solomons test of symmetry by testing if the standardized
#' nonparametric skew (\eqn{\frac{(Mean - Median)}{SD}}) is different than 0.
#' \cr\cr
#' This is an underlying assumption of Wilcoxon signed-rank test.
#'
#' @param x Model or numeric vector
#' @param ... Not used.
#'
#' @examples
#' V <- suppressWarnings(wilcox.test(mtcars$mpg))
#' check_symmetry(V)
#'
#' @export
check_symmetry <- function(x, ...) {
  UseMethod("check_symmetry")
}

#' @export
check_symmetry.numeric <- function(x, ...) {
  x <- x[!is.na(x)]

  m <- mean(x)
  a <- stats::median(x)
  n <- length(x)
  s <- stats::sd(x)

  D <- n * (m - a) / s

  z <- sqrt(2 * n) * (m - a) / s
  out <- stats::pnorm(abs(z), lower.tail = FALSE)
  class(out) <- c("check_symmetry", "numeric")
  attr(out, "object_name") <- substitute(x)
  out
}

#' @export
print.check_symmetry <- function(x, ...) {
  pstring <- insight::format_p(x)
  if (x < 0.05) {
    insight::print_color(sprintf("Warning: Non-symmetry detected (%s).\n", pstring), "red")
  } else {
    insight::print_color(sprintf("OK: Data appears symmetrical (%s).\n", pstring), "green")
  }
  return(invisible(x))
}
