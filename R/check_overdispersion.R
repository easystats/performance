#' @title Check overdispersion of GL(M)M's
#' @name check_overdispersion
#'
#' @description \code{check_overdispersion()} checks generalized linear (mixed) models
#'    for overdispersion.
#'
#' @param x Fitted model of class \code{merMod}, \code{glmmTMB}, \code{glm},
#'    or \code{glm.nb} (package \pkg{MASS}).
#' @param ... Currently not used.
#'
#' @return A list with results from the overdispersion test, like chi-squared
#'  statistics, p-value or dispersion ratio.
#'
#' @details \subsection{Interpretation of the Dispersion Ratio}{
#' If the dispersion ratio is close to one, a poisson model fits well
#' to the data. Dispersion ratios larger than one indicate overdispersion,
#' thus a negative binomial model or similar might fit better to the data.
#' A p-value < .05 indicates overdispersion.
#' }
#' \subsection{Overdispersion in Poisson Models}{
#' For Poisson models, the overdispersion test is based on the code
#' from \cite{Gelman and Hill (2007), page 115}.
#' }
#' \subsection{Overdispersion in Mixed Models}{
#' For \code{merMod}- and \code{glmmTMB}-objects, \code{check_overdispersion()}
#' is based on the code in the \href{http://bbolker.github.io/mixedmodels-misc/glmmFAQ.html}{GLMM FAQ},
#' section \emph{How can I deal with overdispersion in GLMMs?}. Note that
#' this function only returns an \emph{approximate} estimate of an
#' overdispersion parameter, and is probably inaccurate for zero-inflated
#' mixed models (fitted with \code{glmmTMB}). The same code is also used to
#' check overdispersion for negative binomial models.
#' }
#' \subsection{How to fix Overdispersion}{
#' Overdispersion can be fixed by either modelling the dispersion parameter,
#' or by choosing a different distributional family (like Quasi-Poisson,
#' or negative binomial, see \cite{Gelman and Hill (2007), pages 115-116}).
#' }
#'
#' @references \itemize{
#'  \item Bolker B et al. (2017): \href{http://bbolker.github.io/mixedmodels-misc/glmmFAQ.html}{GLMM FAQ.}
#'  \item Gelman, A., & Hill, J. (2007). Data analysis using regression and multilevel/hierarchical models. Cambridge; New York: Cambridge University Press.
#'  }
#'
#'
#' @examples
#' library(glmmTMB)
#' data(Salamanders)
#'
#' m <- glm(count ~ spp + mined, family = poisson, data = Salamanders)
#' check_overdispersion(m)
#'
#' m <- glmmTMB(
#'   count ~ mined + spp + (1 | site),
#'   family = poisson,
#'   data = Salamanders
#' )
#' check_overdispersion(m)
#'
#' @export
check_overdispersion <- function(x, ...) {
  UseMethod("check_overdispersion")
}


#' @importFrom insight get_response
#' @importFrom stats fitted nobs coef pchisq family
#' @export
check_overdispersion.glm <- function(x, ...) {
  # check if we have poisson
  if (!stats::family(x)$family %in% c("poisson", "quasipoisson"))
    stop("Model must be from Poisson-family.", call. = F)

  yhat <- stats::fitted(x)

  n <- stats::nobs(x)
  k <- length(stats::coef(x))

  zi <- (insight::get_response(x) - yhat) / sqrt(yhat)
  chisq <- sum(zi^2)
  ratio <-  chisq / (n - k)
  p.value <- stats::pchisq(chisq, df = n - k, lower.tail = FALSE)

  structure(
    class = "check_overdisp",
    list(
      chisq_statistic = chisq,
      dispersion_ratio = ratio,
      residual_df = n - k,
      p_value = p.value
    )
  )
}


#' @export
check_overdispersion.negbin <- function(x, ...) {
  check_overdispersion.lme4(x)
}


#' @export
check_overdispersion.merMod <- function(x, ...) {
  check_overdispersion.lme4(x)
}


#' @export
check_overdispersion.glmmTMB <- function(x, ...) {
  check_overdispersion.lme4(x)
}


#' @importFrom stats df.residual residuals pchisq
check_overdispersion.lme4 <- function(x) {
  rdf <- stats::df.residual(x)
  rp <- stats::residuals(x, type = "pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq / rdf
  pval <- stats::pchisq(Pearson.chisq, df = rdf, lower.tail = FALSE)

  structure(
    class = "check_overdisp",
    list(
      chisq_statistic = Pearson.chisq,
      dispersion_ratio = prat,
      residual_df = rdf,
      p_value = pval
    )
  )
}
