#' @title Discrimination of Questionnaire Items
#' @name item_discrimination
#'
#' @description Compute various measures of internal consistencies
#'    for tests or item-scales of questionnaires.
#'
#' @param x A matrix or a data frame.
#' @param standardize Logical, if `TRUE`, the data frame's vectors will be
#'   standardized. Recommended when the variables have different measures /
#'   scales.
#'
#' @return A data frame with the item discrimination (*corrected item-total
#'   correlations*) for each item of the scale.
#'
#' @details
#' This function calculates the item discriminations (corrected item-total
#' correlations for each item of `x` with the remaining items) The absolute
#' value of the item discrimination indices should be above 0.2. An index between
#' 0.2 and 0.4 is considered as "fair", while a satisfactory index ranges from
#' 0.4 to 0.7. Items with low discrimination indices are often ambiguously worded
#' and should be examined. Items with negative indices should be examined to
#' determine why a negative value was obtained (e.g. reversed answer categories
#' regarding positive and negative poles).
#'
#' @references
#' - Kelava A, Moosbrugger H (2020). Deskriptivstatistische Itemanalyse und
#'   Testwertbestimmung. In: Moosbrugger H,  Kelava A, editors. Testtheorie und
#'   Fragebogenkonstruktion. Berlin, Heidelberg: Springer, 143–158
#'
#' @examples
#' data(mtcars)
#' x <- mtcars[, c("cyl", "gear", "carb", "hp")]
#' item_discrimination(x)
#' @export
item_discrimination <- function(x, standardize = FALSE) {
  # check param
  if (!is.matrix(x) && !is.data.frame(x)) {
    insight::format_warning("`x` needs to be a data frame or matrix.")
    return(NULL)
  }

  # remove missings, so correlation works
  x <- stats::na.omit(x)

  # remember item (column) names for return value
  # return value gets column names of initial data frame
  df.names <- colnames(x)

  # Check whether items should be scaled. Needed,
  # when items have different measures / scales
  if (standardize) {
    x <- .std(x)
  }
  # calculate corrected total-item correlation
  id <- vapply(seq_len(ncol(x)), function(i) {
    stats::cor(x[, i], apply(x[, -i], 1, sum), use = "pairwise.complete.obs")
  }, numeric(1))

  out <- data.frame(
    Item = df.names,
    Discrimination = id,
    stringsAsFactors = FALSE
  )

  class(out) <- c("item_discrimination", "data.frame")
  out
}

# methods --------------------------------------

#' @export
print.item_discrimination <- function(x, ...) {
  out <- insight::format_table(x, ...)
  cat(insight::export_table(out, caption = c("Item Discrimination", "blue"), ...))
  invisible(x)
}
