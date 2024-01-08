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
mcdonalds_omega.data.frame <- function(x, verbose = TRUE, ...) {
  # remove missings
  .data <- stats::na.omit(x)

  # we need at least two columns for Cronach's Alpha
  if (is.null(ncol(.data)) || ncol(.data) < 2) {
    if (verbose) {
      insight::format_warning("Too few columns in `x` to compute McDonald's Omega.")
    }
    return(NULL)
  }

  # Compute Cronbach's Alpha
  dim(.data)[2] / (dim(.data)[2] - 1) * (1 - sum(apply(.data, 2, stats::var)) / stats::var(rowSums(.data)))
}


#' @export
mcdonalds_omega.matrix <- function(x, verbose = TRUE, ...) {
  mcdonalds_omega(as.data.frame(x), verbose = verbose, ...)
}
