#' @title Reliability Test for Items or Scales
#' @name item_reliability
#'
#' @description Compute various measures of internal consistencies
#'    for tests or item-scales of questionnaires.
#'
#' @param x A matrix or a data frame.
#' @param standardize Logical, if \code{TRUE}, the data frame's vectors will be
#'    standardized. Recommended when the variables have different measures / scales.
#' @param digits Amount of digits for returned values.
#'
#' @return A data frame with the corrected item-total correlations (\emph{item
#'      discrimination}, column \code{item_discrimination}) and Cronbach's Alpha
#'      (if item deleted, column \code{alpha_if_deleted}) for each item
#'      of the scale, or \code{NULL} if data frame had too less columns.
#'
#' @details This function calculates the item discriminations (corrected item-total
#'    correlations for each item of \code{x} with the remaining items) and
#'    the Cronbach's alpha for each item, if it was deleted from the scale.
#'    The absolute value of the item discrimination indices should be
#'    above 0.1. An index between 0.1 and 0.3 is considered as "fair",
#'    while an index above 0.3 (or below -0.3) is "good". Items with
#'    low discrimination indices are often ambiguously worded and
#'    should be examined. Items with negative indices should be
#'    examined to determine why a negative value was obtained (e.g.
#'    reversed answer categories regarding positive and negative poles).
#'
#' @examples
#' data(mtcars)
#' x <- mtcars[, c("cyl", "gear", "carb", "hp")]
#' item_reliability(x)
#' @importFrom stats cor var na.omit
#' @export
item_reliability <- function(x, standardize = FALSE, digits = 3) {
  # check param
  if (!is.matrix(x) && !is.data.frame(x)) {
    warning("`x` needs to be a data frame or matrix.", call. = FALSE)
    return(NULL)
  }

  # remove missings, so correlation works
  x <- stats::na.omit(x)

  # remember item (column) names for return value
  # return value gets column names of initial data frame
  df.names <- colnames(x)
  ret.df <- NULL

  # check for minimum amount of columns can't be less than 3, because the
  # reliability test checks for Cronbach's alpha if a specific item is deleted.
  # If data frame has only two columns and one is deleted, Cronbach's alpha
  # cannot be calculated.

  if (ncol(x) > 2) {
    # Check whether items should be scaled. Needed,
    # when items have different measures / scales
    if (standardize) x <- .std(x)

    # calculate cronbach-if-deleted
    cronbachDeleted <- sapply(seq_len(ncol(x)), function(i) cronbachs_alpha(x[, -i]))

    # calculate corrected total-item correlation
    totalCorr <- sapply(seq_len(ncol(x)), function(i) stats::cor(x[, i], apply(x[, -i], 1, sum), use = "pairwise.complete.obs"))

    ret.df <- data.frame(
      term = df.names,
      alpha_if_deleted = round(cronbachDeleted, digits),
      item_discrimination = round(totalCorr, digits),
      stringsAsFactors = FALSE
    )
  } else {
    warning("Data frame needs at least three columns for reliability-test.", call. = FALSE)
  }

  ret.df
}
