#' @title Convergence test for mixed effects models
#' @name check_convergence
#'
#' @description \code{check_convergence()} provides an alternative convergence
#'   test for \code{\link[lme4]{merMod}}-objects.
#'
#' @param x A \code{merMod}-object.
#' @param tolerance Indicates up to which value the convergence result is
#'          accepted. The smaller \code{tolerance} is, the stricter the test
#'          will be.
#'
#' @return \code{TRUE} if convergence is fine and \code{FALSE} if convergence
#'   is suspicious. Additionally, the convergence value is returned as attribute.
#'
#' @details \code{check_convergence()} provides an alternative convergence test for
#'   \code{\link[lme4]{merMod}}-objects, as discussed
#'   \href{https://github.com/lme4/lme4/issues/120}{here}
#'   and suggested by Ben Bolker in
#'   \href{https://github.com/lme4/lme4/issues/120#issuecomment-39920269}{this comment}.
#'   Further details can be found in \code{\link[lme4]{convergence}}.
#'
#' @examples
#' library(lme4)
#' data(cbpp)
#' set.seed(1)
#' cbpp$x <- rnorm(nrow(cbpp))
#' cbpp$x2 <- runif(nrow(cbpp))
#'
#' model <- glmer(
#'   cbind(incidence, size - incidence) ~ period + x + x2 + (1 + x | herd),
#'   data = cbpp,
#'   family = binomial()
#' )
#'
#' check_convergence(model)
#'
#' @export
check_convergence <- function(x, tolerance = 0.001) {
  # check for package availability
  if (!requireNamespace("Matrix", quietly = TRUE))
    stop("Package `Matrix` needed for this function to work. Please install it.", call. = FALSE)

  # is 'x' an lmer object?
  if (!inherits(x, "merMod"))
    warning("`x` must be a `merMod` object.", call. = F)


  relgrad <- with(x@optinfo$derivs, Matrix::solve(Hessian, gradient))

  # copy logical value, TRUE if convergence is OK
  retval <- max(abs(relgrad)) < tolerance
  # copy convergence value
  attr(retval, "gradient") <- max(abs(relgrad))

  # return result
  retval
}
