#' @title Compute the AIC or second-order AIC
#' @name performance_aicc
#'
#' @description
#'
#' Compute the AIC or the second-order Akaike's information criterion (AICc).
#' `performance_aic()` is a small wrapper that returns the AIC, however, for
#' models with a transformed response variable, `performance_aic()` returns the
#' corrected AIC value (see 'Examples'). It is a generic function that also
#' works for some models that don't have a AIC method (like Tweedie models).
#' `performance_aicc()` returns the second-order (or "small sample") AIC that
#' incorporates a correction for small sample sizes.
#'
#' @param x A model object.
#' @param estimator Only for linear models. Corresponds to the different
#'   estimators for the standard deviation of the errors. If `estimator = "ML"`
#'   (default), the scaling is done by n (the biased ML estimator), which is
#'   then equivalent to using `AIC(logLik())`. Setting it to `"REML"` will give
#'   the same results as `AIC(logLik(..., REML = TRUE))`.
#' @param verbose Toggle warnings.
#' @param ... Currently not used.
#'
#' @return Numeric, the AIC or AICc value.
#'
#' @details `performance_aic()` correctly detects transformed response and,
#' unlike `stats::AIC()`, returns the corrected AIC value.
#'
#' @references
#' - Akaike, H. (1973) Information theory as an extension of the maximum
#' likelihood principle. In: Second International Symposium on Information
#' Theory, pp. 267-281. Petrov, B.N., Csaki, F., Eds, Akademiai Kiado, Budapest.
#'
#' - Hurvich, C. M., Tsai, C.-L. (1991) Bias of the corrected AIC criterion
#' for underfitted regression and time series models. Biometrika 78, 499–509.
#'
#' @examples
#' m <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
#' AIC(m)
#' performance_aicc(m)
#'
#' # correct AIC for models with transformed response variable
#' data("mtcars")
#' mtcars$mpg <- floor(mtcars$mpg)
#' model <- lm(log(mpg) ~ factor(cyl), mtcars)
#'
#' # wrong AIC, not corrected for log-transformation
#' AIC(model)
#'
#' # performance_aic() correctly detects transformed response and
#' # returns corrected AIC
#' performance_aic(model)
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

#' @rdname performance_aicc
#' @export
performance_aic.default <- function(x, estimator = "ML", verbose = TRUE, ...) {
  info <- suppressWarnings(insight::model_info(x))

  # check ML estimator
  REML <- identical(estimator, "REML")
  if (isTRUE(list(...)$REML)) REML <- TRUE

  # special handling for tweedie
  if (info$family == "Tweedie") {
    insight::check_if_installed("tweedie")
    aic <- suppressMessages(tweedie::AICtweedie(x))
  } else {
    # all other models...
    aic <- tryCatch(
      stats::AIC(insight::get_loglikelihood(x, check_response = TRUE, REML = REML, verbose = verbose)),
      error = function(e) NULL
    )
  }
  aic
}


# mixed models ------------------------------------


#' @export
performance_aic.lmerMod <- function(x, estimator = "REML", verbose = TRUE, ...) {
  REML <- identical(estimator, "REML")
  if (isFALSE(list(...)$REML)) REML <- FALSE

  tryCatch(
    stats::AIC(insight::get_loglikelihood(x, check_response = TRUE, REML = REML, verbose = verbose)),
    error = function(e) NULL
  )
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
  aic <- tryCatch(stats::AIC(x)[["AIC"]], error = function(e) NULL)
  .adjust_ic_jacobian(x, aic)
}

#' @export
performance_aic.svycoxph <- performance_aic.svyglm



# mfx models --------------------------------------

#' @export
performance_aic.logitor <- function(x, ...) {
  performance_aic(x$fit, ...)
}

# styler: off

#' @export
performance_aic.logitmfx   <- performance_aic.logitor

#' @export
performance_aic.probitmfx  <- performance_aic.logitor

#' @export
performance_aic.negbinirr  <- performance_aic.logitor

#' @export
performance_aic.negbinmfx  <- performance_aic.logitor

#' @export
performance_aic.betaor     <- performance_aic.logitor

#' @export
performance_aic.betamfx    <- performance_aic.logitor

#' @export
performance_aic.poissonirr <- performance_aic.logitor

#' @export
performance_aic.poissonmfx <- performance_aic.logitor

#' @export

# styler: on

# Other models --------------------------------------

#' @export
performance_aic.bayesx <- function(x, ...) {
  out <- stats::AIC(x)[["AIC"]]
  .adjust_ic_jacobian(x, out)
}

# methods ------------------------------------------

#' @export
AIC.bife <- function(object, ..., k = 2) {
  -2 * as.numeric(insight::get_loglikelihood(object)) + k * insight::get_df(object, type = "model")
}


# AICc ------------------------------------------


#' @export
performance_aicc.default <- function(x, estimator = "ML", ...) {
  # check ML estimator
  REML <- identical(estimator, "REML")
  if (isTRUE(list(...)$REML)) REML <- TRUE

  n <- suppressWarnings(insight::n_obs(x))
  ll <- insight::get_loglikelihood(x, check_response = TRUE, REML = REML, verbose = TRUE)
  k <- attr(ll, "df")

  -2 * as.vector(ll) + 2 * k * (n / (n - k - 1))
}


#' @export
performance_aicc.lmerMod <- function(x, estimator = "REML", ...) {
  REML <- identical(estimator, "REML")
  if (isFALSE(list(...)$REML)) REML <- FALSE

  n <- suppressWarnings(insight::n_obs(x))
  ll <- insight::get_loglikelihood(x, check_response = TRUE, REML = REML, verbose = TRUE)
  k <- attr(ll, "df")

  -2 * as.vector(ll) + 2 * k * (n / (n - k - 1))
}


#' @export
performance_aicc.bife <- function(x, ...) {
  n <- suppressWarnings(insight::n_obs(x))
  ll <- insight::get_loglikelihood(x, check_response = TRUE)
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




# jacobian / derivate for log models and other transformations ----------------


# this function adjusts any IC for models with transformed response variables
.adjust_ic_jacobian <- function(model, ic) {
  response_transform <- insight::find_transformation(model)
  if (!is.null(ic) && !is.null(response_transform) && !identical(response_transform, "identity")) {
    adjustment <- tryCatch(.ll_jacobian_adjustment(model, insight::get_weights(model, na_rm = TRUE)), error = function(e) NULL)
    if (!is.null(adjustment)) {
      ic <- ic - 2 * adjustment
    }
  }
  ic
}


# this function calculates the adjustment for the log-likelihood of a model
# with transformed response
.ll_jacobian_adjustment <- function(model, weights = NULL) {
  tryCatch(
    {
      trans <- insight::get_transformation(model)$transformation
      .weighted_sum(log(
        diag(attr(with(
          insight::get_data(model),
          stats::numericDeriv(
            expr = quote(trans(
              get(insight::find_response(model))
            )),
            theta = insight::find_response(model)
          )
        ), "gradient"))
      ), weights)
    },
    error = function(e) {
      NULL
    }
  )
}

.weighted_sum <- function(x, w = NULL, ...) {
  if (is.null(w)) {
    mean(x) * length(x)
  } else {
    stats::weighted.mean(x, w) * length(x)
  }
}
