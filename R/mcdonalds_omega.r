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
  q <- length(varnames)
  N <- nrow(x)
  loadingName <- paste0("a", 1:q)
  errorName <- paste0("b", 1:q)

  model <- paste0("f1 =~ NA*", varnames[1], " + ")
  loadingLine <- paste(paste0(loadingName, "*", varnames), collapse = " + ")
  factorLine <- "f1 ~~ 1*f1\n"
  errorLine <- paste(paste0(varnames, " ~~ ", errorName, "*", varnames), collapse = "\n")
  sumLoading <- paste("loading :=", paste(loadingName, collapse = " + "), "\n")
  sumError <- paste("error :=", paste(errorName, collapse = " + "), "\n")
  relia <- "relia := (loading^2) / ((loading^2) + error) \n"
  model <- paste0(
    model,
    loadingLine,
    "\n",
    factorLine,
    errorLine,
    "\n",
    sumLoading,
    sumError,
    relia
  )

  fit <- lavaan::cfa(model,
    data = attitude[, -1], missing = "ml", estimator = "mlr", se = "default"
  )

  lavaan::parameterEstimates(fit)

  est <- 0.8274243
  se <- 0.05258224

  crit <- stats::qnorm((1 + ci) / 2)

  logest <- log(est / (1 - est))
  logse <- se / (est * (1 - est))
  loglower <- logest - crit * logse
  logupper <- logest + crit * logse
  if (logupper < loglower) {
    temp <- loglower
    loglower <- logupper
    loguppper <- temp
  }
  lower <- 1 / (1 + exp(-loglower))
  upper <- 1 / (1 + exp(-logupper))
  c(lower, upper)
}


#' @export
mcdonalds_omega.matrix <- function(x, verbose = TRUE, ...) {
  mcdonalds_omega(as.data.frame(x), verbose = verbose, ...)
}
