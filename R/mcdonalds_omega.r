#' @title McDonald's Omega for Items or Scales
#' @name mcdonalds_omega
#'
#' @description Compute various measures of internal consistencies
#'    for tests or item-scales of questionnaires.
#'
#' @param ci Confidence interval for the reliability estimate. If `NULL`,
#' no confidence interval is computed.
#' @inheritParams cronbachs_alpha
#'
#' @return The McDonald's Omega value for `x`.
#'
#' @details The McDonald's Omega value for `x`. A value closer to 1
#'    indicates greater internal consistency, where usually following
#'    rule of thumb is applied to interpret the results:
#'    \ifelse{html}{\out{&omega;}}{\eqn{\omega}{omega}} < 0.5 is unacceptable,
#'    0.5 < \ifelse{html}{\out{&omega;}}{\eqn{\omega}{omega}} < 0.6 is poor,
#'    0.6 < \ifelse{html}{\out{&omega;}}{\eqn{\omega}{omega}} < 0.7 is questionable,
#'    0.7 < \ifelse{html}{\out{&omega;}}{\eqn{\omega}{omega}} < 0.8 is acceptable,
#'    and everything > 0.8 is good or excellent.
#'
#' `mcdonalds_omega()` is a simplified implementation of the `MBESS::ci.reliability()`
#' function. Currently, it only computes the simple McDonald's Omega estimate
#' (not hierarchical, not for categorical data) and should return the same
#' results as the default `MBESS::ci.reliability()` call.
#'
#' @note The code is based on the `MBESS::ci.reliability()` function, which
#' is licensed under the GPL-2|GPL-3 license. Credits go to Sunthud Pornprasertmanit
#' and Ken Kelley.
#'
#' @references McDonald, R.P. (1999). Test theory: A unified treatment. Hillsdale: Erlbaum.
#'
#' @examplesIf requireNamespace("lavaan", quietly = TRUE)
#' data(iris)
#' x <- iris[1:3]
#' mcdonalds_omega(x)
#' @export
mcdonalds_omega <- function(x, ...) {
  UseMethod("mcdonalds_omega")
}


#' @rdname mcdonalds_omega
#' @export
mcdonalds_omega.data.frame <- function(x, ci = 0.95, verbose = TRUE, ...) {
  # we need at least two columns for Cronach's Alpha
  if (is.null(ncol(x)) || ncol(x) < 2) {
    if (verbose) {
      insight::format_warning("Too few columns in `x` to compute McDonald's Omega.")
    }
    return(NULL)
  }

  # prepare names and formulas for lavaan
  varnames <- colnames(x)
  name_loadings <- paste0("a", seq_len(ncol(x)))
  name_error <- paste0("b", seq_len(ncol(x)))

  # we need this specific formulation for lavaan to get the omega reliability estimate
  # see code in MBESS
  model <- paste0("f1 =~ NA*", varnames[1], " + ")
  formula_loadings <- paste(paste0(name_loadings, "*", varnames), collapse = " + ")
  formula_factors <- "f1 ~~ 1*f1\n"
  formula_error <- paste(paste0(varnames, " ~~ ", name_error, "*", varnames), collapse = "\n")
  formula_sum_loadings <- paste("loading :=", paste(name_loadings, collapse = " + "), "\n")
  formula_sum_error <- paste("error :=", paste(name_error, collapse = " + "), "\n")
  formula_reliability <- "relia := (loading^2) / ((loading^2) + error) \n"

  model <- paste0(
    model,
    formula_loadings,
    "\n",
    formula_factors,
    formula_error,
    "\n",
    formula_sum_loadings,
    formula_sum_error,
    formula_reliability
  )

  insight::check_if_installed("lavaan")

  # fit CFA to get reliability estimate
  fit <- .safe(suppressWarnings(lavaan::cfa(model, data = x, missing = "ml", estimator = "mlr", se = "default")))
  if (is.null(fit)) {
    if (verbose) {
      insight::format_warning("Could not compute McDonald's Omega.")
    }
    return(NULL)
  }
  out <- suppressWarnings(lavaan::parameterEstimates(fit))

  # extract omega and related standard error
  estimate <- as.vector(out$est[out$label == "relia"])
  se <- as.vector(out$se[out$label == "relia"])

  # check if omega is in range
  if ((estimate < 0 || estimate > 1)) {
    if (!is.null(ci) && !is.na(ci)) {
      if (verbose) {
        insight::format_warning("McDonald's Omega is not in range [0, 1]. Estimate is not reliable. Furthermore, can't compute confidence intervals.") # nolint
      }
      ci <- NULL
    } else if (verbose) {
      if (estimate < 0) {
        insight::format_warning("McDonald's Omega is negativ. Estimate is not reliable.")
      } else {
        insight::format_warning("McDonald's Omega is greater than 1. Estimate is not reliable.")
      }
    }
  }

  # if user requested CI, return data frame with omega and CI
  if (!is.null(ci) && !is.na(ci)) {
    crit <- stats::qnorm((1 + ci) / 2)

    logest <- log(estimate / (1 - estimate))
    logse <- se / (estimate * (1 - estimate))
    loglower <- logest - crit * logse
    logupper <- logest + crit * logse

    if (logupper < loglower) {
      temp <- loglower
      loglower <- logupper
      loguppper <- temp
    }

    omega <- data.frame(
      Omega = estimate,
      CI_low = 1 / (1 + exp(-loglower)),
      CI_high = 1 / (1 + exp(-logupper)),
      stringsAsFactors = FALSE
    )
    class(omega) <- c("mcdonalds_omega", "data.frame")
  } else {
    omega <- estimate
  }

  omega
}


#' @export
mcdonalds_omega.matrix <- function(x, ci = 0.95, verbose = TRUE, ...) {
  mcdonalds_omega(as.data.frame(x), ci = ci, verbose = verbose, ...)
}


#' @export
mcdonalds_omega.parameters_pca <- function(x, verbose = TRUE, ...) {
  # fetch data used for the PCA
  pca_data <- attributes(x)$dataset

  # if NULL, can we get from environment?
  if (is.null(pca_data)) {
    pca_data <- attr(x, "data")
    if (is.null(pca_data)) {
      if (verbose) {
        insight::format_warning("Could not find data frame that was used for the PCA.")
      }
      return(NULL)
    }
    pca_data <- get(pca_data, envir = parent.frame())
  }

  # get assignment of columns to extracted components, based on the max loading
  factor_assignment <- attributes(x)$closest_component

  # sort and get unique IDs so we only get data from relevant columns
  unique_factors <- sort(unique(factor_assignment))

  # apply mcdonalds_omega for each component,
  # only for variables with max loading
  omegas <- sapply(unique_factors, function(i) {
    mcdonalds_omega(
      pca_data[, as.vector(x$Variable[factor_assignment == i]), drop = FALSE],
      ci = NULL,
      verbose = verbose,
      ...
    )
  })

  names(omegas) <- paste0("PC", unique_factors)
  unlist(omegas)
}


# methods ---------------------------------------------------------------------

#' @export
print.mcdonalds_omega <- function(x, digits = 3, ...) {
  # print regular R2
  out <- sprintf(
    "McDonald's Omega: %.*f %s",
    digits,
    x$Omega,
    insight::format_ci(x$CI_low, x$CI_high, digits = digits, ci = NULL)
  )

  cat(out)
  cat("\n")
  invisible(x)
}
