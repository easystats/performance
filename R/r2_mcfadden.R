#' McFadden's (1973) R2
#'
#' DESCRIPTION TO BE IMPROVED.
#'
#' @param model Multinomial Logit (\code{mlogit}) Model.
#'
#'
#' @references
#' \itemize{
#'   \item McFadden, D. (1987). Regression-based specification tests for the multinomial logit model. Journal of econometrics, 34(1-2), 63-82.
#'   \item McFadden, D. (1973). Conditional logit analysis of qualitative choice behavior.
#' }
#' @export
r2_mcfadden <- function(model) {
  UseMethod("r2_mcfadden")
}




#' @export
r2_mcfadden.mlogit <- function(model) {
  R2 <- as.vector(summary(model)$mfR2)
  names(R2) <- "McFadden's R2"
  R2
}
