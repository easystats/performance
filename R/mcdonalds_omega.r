#' @title McDonald's Omega for Items or Scales
#' @name mcdonalds_omega
#'
#' @description Compute various measures of internal consistencies
#'    for tests or item-scales of questionnaires.
#'
#' @param x A matrix or a data frame.
#' @param ... Currently not used.
#'
#' @return The McDonald's Omega value for `x`.
#'
#' @details The McDonald's Omega value for `x`. A value closer to 1
#'    indicates greater internal consistency, where usually following
#'    rule of thumb is applied to interpret the results:
#'    \ifelse{html}{\out{&alpha;}}{\eqn{\alpha}{alpha}} < 0.5 is unacceptable,
#'    0.5 < \ifelse{html}{\out{&alpha;}}{\eqn{\alpha}{alpha}} < 0.6 is poor,
#'    0.6 < \ifelse{html}{\out{&alpha;}}{\eqn{\alpha}{alpha}} < 0.7 is questionable,
#'    0.7 < \ifelse{html}{\out{&alpha;}}{\eqn{\alpha}{alpha}} < 0.8 is acceptable,
#'    and everything > 0.8 is good or excellent.
#'
#' @references Bland, J. M., and Altman, D. G. Statistics notes: Cronbach's
#'   alpha. BMJ 1997;314:572. 10.1136/bmj.314.7080.572
#'
#' @examples
#' data(mtcars)
#' x <- mtcars[, c("cyl", "gear", "carb", "hp")]
#' mcdonalds_omega(x)
#' @export
mcdonalds_omega <- function(x, ...) {
  UseMethod("mcdonalds_omega")
}


#' @export
mcdonalds_omega.data.frame <- function(x, ci = 0.95, verbose = TRUE, ...) {
  varnames <- colnames(x)
  n_params <- length(varnames)
  name_loadings <- paste0("a", 1:n_params)
  name_error <- paste0("b", 1:n_params)

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

  fit <- lavaan::cfa(model, data = x, missing = "ml", estimator = "mlr", se = "default")
  out <- lavaan::parameterEstimates(fit)

  estimate <- as.vector(out$est[out$label == "relia"])
  se <- as.vector(out$se[out$label == "relia"])

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
    ci_low <- 1 / (1 + exp(-loglower))
    ci_high <- 1 / (1 + exp(-logupper))
  } else {
    ci_low <- NA
    ci_high <- NA
  }
}


#' @export
mcdonalds_omega.matrix <- function(x, verbose = TRUE, ...) {
  mcdonalds_omega(as.data.frame(x), verbose = verbose, ...)
}
