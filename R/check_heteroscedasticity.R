#' @title Check model for (non-)constant error variance
#' @name check_heteroscedasticity
#'
#' @description Check model for (non-)constant error variance.
#'
#' @param x A model object.
#' @param ... Currently not used.
#'
#' @return Invisibly returns the p-value of the test statistics. A p-value < 0.05
#' indicates a non-constant variance (heteroskedasticity).
#'
#' @examples
#' m <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
#' check_heteroscedasticity(m)
#' plot(check_heteroscedasticity(m))
#'
#' @importFrom stats residuals df.residual fitted anova pchisq
#' @importFrom insight print_color
#' @export
check_heteroscedasticity <- function(x, ...) {
  UseMethod("check_heteroscedasticity")
}


#' @export
check_heteroscedasticity.default <- function(x, ...) {
  sumry <- summary(x)
  r <- stats::residuals(x, type = "pearson")
  S.sq <- stats::df.residual(x) * (sumry$sigma)^2 / sum(!is.na(r))

  .U <- (r^2) / S.sq
  mod <- lm(.U ~ stats::fitted(x))

  SS <- stats::anova(mod)$"Sum Sq"
  RegSS <- sum(SS) - SS[length(SS)]
  Chisq <- RegSS / 2

  p.val <- stats::pchisq(Chisq, df = 1, lower.tail = FALSE)

  if (p.val < 0.05) {
    insight::print_color(sprintf("Warning: Heteroscedasticity (non-constant error variance) detected (p = %.3f).\n", p.val), "red")
  } else {
    insight::print_color(sprintf("OK: Error variance appears to be homoscedastic (p = %.3f).\n", p.val), "green")
  }

  attr(p.val, "object_name") <- deparse(substitute(x), width.cutoff = 500)
  class(p.val) <- unique(c("check_heteroscedasticity", "see_check_heteroscedasticity", class(p.val)))

  invisible(p.val)
}
