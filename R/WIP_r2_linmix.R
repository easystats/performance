#' Internal for now (WIP)
#'
#' @importFrom insight get_response
#' @importFrom stats var fitted residuals lm
#' @keywords internal
r2_linmix <- function(x, n) {
  # do we have null model?
  if (!is.null(n)) {
    # compute tau for both models

    ## TODO needs icc function!

    tau_full <- suppressMessages(icc(x))
    tau_null <- suppressMessages(icc(n))

    # get taus. tau.00 is the random intercept variance, i.e. for growth models,
    # the difference in the outcome's mean at first time point
    rsq0 <- (attr(tau_null, "tau.00") - attr(tau_full, "tau.00")) / attr(tau_null, "tau.00")

    # tau.11 is the variance of the random slopes, i.e. how model predictors
    # affect the trajectory of subjects over time (for growth models)
    rsq1 <- (attr(tau_null, "tau.11") - attr(tau_full, "tau.11")) / attr(tau_null, "tau.11")

    # get r2
    rsq <- ((attr(tau_null, "tau.00") + attr(tau_null, "sigma_2")) -
              (attr(tau_full, "tau.00") + attr(tau_full, "sigma_2"))) /
      (attr(tau_null, "tau.00") + attr(tau_null, "sigma_2"))

    # get omega-squared
    osq <- 1 - ((attr(tau_full, "sigma_2") / attr(tau_null, "sigma_2")))

    # if model has no random slope, we need to set this value to NA
    if (is.null(rsq1) || .is_empty_object(rsq1)) rsq1 <- NA

    # name vectors
    names(rsq0) <- "R-squared (tau-00)"
    names(rsq1) <- "R-squared (tau-11)"
    names(rsq) <- "R-squared"
    names(osq) <- "Omega-squared"

    # return results
    structure(class = "sj_r2", list(
      r2_tau00 = rsq0,
      r2_tau11 = rsq1,
      r2 = rsq,
      o2 = osq
    ))
  } else {
    # compute "correlation"
    lmfit <-  lm(resp_val(x) ~ stats::fitted(x))
    # get r-squared
    rsq <- summary(lmfit)$r.squared
    # get omega squared
    osq <- 1 - stats::var(stats::residuals(x)) / stats::var(insight::get_response(x))

    # name vectors
    names(rsq) <- "R-squared"
    names(osq) <- "Omega-squared"

    # return results

    ## TODO return class attribute?

    structure(class = "sj_r2", list(r2 = rsq, o2 = osq))
  }
}
