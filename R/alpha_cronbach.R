#' @title Cronbach's Alpha for Items or Scales
#' @name alpha_cronbach
#'
#' @description Compute various measures of internal consistencies
#'    for tests or item-scales of questionnaires.
#'
#' @param x A matrix or a data frame.
#'
#' @return The Cronbach's Alpha value for \code{x}.
#'
#' @details The Cronbach's Alpha value for \code{x}. A value closer to 1
#'    indicates greater internal consistency, where usually following
#'    rule of thumb is applied to interprete the results:
#'    \ifelse{html}{\out{&alpha;}}{\eqn{\alpha}{alpha}} < 0.5 is unacceptable,
#'    0.5 < \ifelse{html}{\out{&alpha;}}{\eqn{\alpha}{alpha}} < 0.6 is poor,
#'    0.6 < \ifelse{html}{\out{&alpha;}}{\eqn{\alpha}{alpha}} < 0.7 is questionable,
#'    0.7 < \ifelse{html}{\out{&alpha;}}{\eqn{\alpha}{alpha}} < 0.8 is acceptable,
#'    and everything > 0.8 is good or excellent.
#'
#' @examples
#' data(mtcars)
#' x <- mtcars[, c("cyl", "gear", "carb", "hp")]
#' alpha_cronbach(x)
#'
#' @importFrom stats var na.omit
#' @export
alpha_cronbach <- function(x) {
  # remove missings
  .data <- stats::na.omit(x)

  # we need at least two columns for Cronach's Alpha
  if (is.null(ncol(.data)) || ncol(.data) < 2) {
    warning("Too less columns in `x` to compute Cronbach's Alpha.", call. = F)
    return(NULL)
  }

  # Compute Cronbach's Alpha
  dim(.data)[2] / (dim(.data)[2] - 1) * (1 - sum(apply(.data, 2, stats::var)) / stats::var(rowSums(.data)))
}
