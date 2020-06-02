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
#' @details Overdispersion occurs when the observed variance is higher than
#' the variance of a theoretical model. For Poisson models, variance increases
#' with the mean, thus, variance usually (roughly) equals the mean value. If
#' the variance is much higher, the data are "overdispersed".
#' \subsection{Interpretation of the Dispersion Ratio}{
#' If the dispersion ratio is close to one, a Poisson model fits well
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
#' mixed models (fitted with \code{glmmTMB}).
#' }
#' \subsection{How to fix Overdispersion}{
#' Overdispersion can be fixed by either modeling the dispersion parameter,
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
#' if (require("glmmTMB")) {
#'   data(Salamanders)
#'   m <- glm(count ~ spp + mined, family = poisson, data = Salamanders)
#'   check_overdispersion(m)
#'
#'   m <- glmmTMB(
#'     count ~ mined + spp + (1 | site),
#'     family = poisson,
#'     data = Salamanders
#'   )
#'   check_overdispersion(m)
#' }
#' @export
check_overdispersion <- function(x, ...) {
  UseMethod("check_overdispersion")
}




# Overdispersion for classical models -----------------------------


#' @importFrom insight get_response model_info find_parameters
#' @importFrom stats fitted nobs coef pchisq
#' @export
check_overdispersion.glm <- function(x, ...) {
  # check if we have poisson
  model_info <- insight::model_info(x)
  if (!model_info$is_poisson) {
    stop("Model must be from Poisson-family.", call. = F)
  }

  yhat <- stats::fitted(x)

  n <- stats::nobs(x)
  k <- length(insight::find_parameters(x, effects = "fixed", flatten = TRUE))

  zi <- (insight::get_response(x) - yhat) / sqrt(yhat)
  chisq <- sum(zi^2)
  ratio <- chisq / (n - k)
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
check_overdispersion.fixest <- check_overdispersion.glm

#' @export
check_overdispersion.glmx <- check_overdispersion.glm




# mfx models ------------------------------

#' @export
check_overdispersion.poissonmfx <- function(x, ...) {
  check_overdispersion(x$fit, ...)
}

#' @export
check_overdispersion.poissonirr <- check_overdispersion.poissonmfx

#' @export
check_overdispersion.negbinirr <- check_overdispersion.poissonmfx

#' @export
check_overdispersion.negbinmfx <- check_overdispersion.poissonmfx





# Overdispersion for mixed models ---------------------------


#' @importFrom insight model_info
#' @importFrom stats df.residual residuals pchisq
#' @export
check_overdispersion.merMod <- function(x, ...) {
  # check if we have poisson
  if (!insight::model_info(x)$is_poisson) {
    stop("Model must be from Poisson-family.", call. = F)
  }

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

#' @export
check_overdispersion.negbin <- check_overdispersion.merMod

#' @export
check_overdispersion.glmmTMB <- check_overdispersion.merMod
