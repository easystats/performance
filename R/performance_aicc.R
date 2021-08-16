#' @title Compute the AIC or second-order AIC
#' @name performance_aicc
#'
#' @description Compute the AIC or the second-order Akaike's information criterion (AICc).
#' `performance_aic()` is a small wrapper that returns the AIC. It is
#' a generic function that also works for some models that don't have a AIC method
#' (like Tweedie models). `performance_aicc()` returns the second-order (or "small sample") AIC that incorporates a correction for small sample sizes.
#'
#' @param x A model object.
#' @param ... Currently not used.
#'
#' @return Numeric, the AIC or AICc value.
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
#' @export
performance_aicc <- function(x, ...) {
  UseMethod("performance_aicc")
}

#' @rdname performance_aicc
#' @export
performance_aic <- function(x, ...) {
  UseMethod("performance_aic")
}


# default -------------------------------------------------

#' @export
performance_aic.default <- function(x, ...) {
  info <- suppressWarnings(insight::model_info(x))
  aic <- NULL

  ## TODO remove is.list() once insight 0.8.3 is on CRAN
  if (is.null(info) || !is.list(info)) {
    info <- list(family = "unknown")
  }

  if (info$family == "Tweedie") {
    insight::check_if_installed("tweedie")
    aic <- suppressMessages(tweedie::AICtweedie(x))
  } else {
    aic <- tryCatch(
      {
        stats::AIC(x)
      },
      error = function(e) {
        NULL
      }
    )

    if (is.null(aic)) {
      aic <- tryCatch(
        {
          -2 * as.numeric(insight::get_loglikelihood(x)) + 2 * insight::get_df(x, type = "model")
        },
        error = function(e) {
          NULL
        }
      )
    }
  }

  aic
}


# VGAM models ------------------------------------


#' @export
performance_aic.vgam <- function(x, ...) {
  insight::check_if_installed("VGAM")
  VGAM::AIC(x)
}

#' @export
performance_aic.vglm <- performance_aic.vgam



# Survey models --------------------------------------

#' @export
performance_aic.svyglm <- function(x, ...) {
  tryCatch(
    {
      stats::AIC(x)[["AIC"]]
    },
    error = function(e) {
      NULL
    }
  )
}

#' @export
performance_aic.svycoxph <- performance_aic.svyglm



# mfx models --------------------------------------

#' @export
performance_aic.logitor <- function(x, ...) {
  performance_aic(x$fit, ...)
}

#' @export
performance_aic.logitmfx <- performance_aic.logitor

#' @export
performance_aic.probitmfx <- performance_aic.logitor

#' @export
performance_aic.poissonirr <- performance_aic.logitor

#' @export
performance_aic.poissonmfx <- performance_aic.logitor

#' @export
performance_aic.negbinirr <- performance_aic.logitor

#' @export
performance_aic.negbinmfx <- performance_aic.logitor

#' @export
performance_aic.betaor <- performance_aic.logitor

#' @export
performance_aic.betamfx <- performance_aic.logitor


# Other models --------------------------------------

#' @export
performance_aic.bayesx <- function(x, ...) {
  stats::AIC(x)[["AIC"]]
}

# methods ------------------------------------------

#' @export
AIC.bife <- function(object, ..., k = 2) {
  -2 * as.numeric(insight::get_loglikelihood(object)) + k * insight::get_df(object, type = "model")
}


# AICc ------------------------------------------


#' @export
performance_aicc.default <- function(x, ...) {
  n <- suppressWarnings(insight::n_obs(x))
  ll <- insight::get_loglikelihood(x)
  k <- attr(ll, "df")

  -2 * as.vector(ll) + 2 * k * (n / (n - k - 1))
}


#' @export
performance_aicc.bife <- function(x, ...) {
  n <- suppressWarnings(insight::n_obs(x))
  ll <- insight::get_loglikelihood(x)
  nparam <- length(insight::find_parameters(x, effects = "fixed", flatten = TRUE))
  k <- n - nparam
  -2 * as.vector(ll) + 2 * k * (n / (n - k - 1))
}

#' @export
performance_aicc.Arima <- performance_aicc.bife


#' @export
performance_aicc.vglm <- function(x, ...) {
  insight::check_if_installed("VGAM")
  VGAM::AICc(x)
}


#' @export
performance_aicc.rma <- function(x, ...) {
  stats::AIC(x, correct = TRUE)
}
