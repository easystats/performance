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


.r2_mcfadden <- function(model, l_base) {
  l_full <- stats::logLik(model)
  mcfadden <- 1 - (l_full/l_base)

  names(mcfadden) <- "McFadden's R2"
  mcfadden
}


#' @export
r2_mcfadden.glm <- function(model) {
  l_base <- stats::logLik(stats::update(model, ~1))
  .r2_mcfadden(model, l_base)
}


#' @export
r2_mcfadden.vglm <- function(model) {
  if (!(is.null(model@call$summ) && !identical(model@call$summ, 0))) {
    stop("Can't get log-likelihood when `summ` is not zero.", call. = FALSE)
  }

  l_base <- stats::logLik(stats::update(model, ~1))
  .r2_mcfadden(model, l_base)
}


#' @export
r2_mcfadden.clm <- function(model) {
  l_base <- stats::logLik(stats::update(model, ~1))
  .r2_mcfadden(model, l_base)
}


#' @export
r2_mcfadden.clm2 <- function(model) {
  l_base <- stats::logLik(stats::update(model, location = ~1, scale = ~1))
  .r2_mcfadden(model, l_base)
}


#' @export
r2_mcfadden.polr <- function(model) {
  l_base <- stats::logLik(stats::update(model, ~1))
  .r2_mcfadden(model, l_base)
}


#' @export
r2_mcfadden.multinom <- function(model) {
  l_base <- stats::logLik(stats::update(model, ~1, trace = FALSE))
  .r2_mcfadden(model, l_base)
}


#' @export
r2_mcfadden.mlogit <- function(model) {
  R2 <- as.vector(summary(model)$mfR2)
  names(R2) <- "McFadden's R2"
  R2
}
