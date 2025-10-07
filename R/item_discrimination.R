#' @title Discrimination and Item-Total Correlation of Questionnaire Items
#' @name item_discrimination
#'
#' @description Compute various measures of internal consistencies for tests or
#' item-scales of questionnaires. `item_discrimination()` calculates the
#' corrected item-total correlations for each item of `x` with the remaining
#' items. `item_totalcor()` by default calculates the item-total correlations
#' (without correction).
#'
#' @param x A matrix or a data frame.
#' @param standardize Logical, if `TRUE`, the data frame's vectors will be
#' standardized. Recommended when the variables have different measures /
#' scales.
#' @param corrected Logical, if `TRUE`, the item-total correlations are corrected
#' for the item itself (default). If `FALSE`, the item-total correlations are
#' calculated without correction.
#' @param verbose Toggle warnings and messages.
#'
#' @return A data frame with the item discrimination (*corrected item-total
#' correlations*) for each item of the scale.
#'
#' @details
#' `item_totalcor()` calculates the item-total correlations (without
#' correction). A positive item-total correlation indicates that an item
#' successfully aligns with the overall test, with higher values signifying a
#' better fit. Conversely, a value near zero suggests the item is not measuring
#' the intended construct, while a negative correlation is a major red flag that
#' the item is flawed, miskeyed, or measures the opposite of what is intended.
#' This means a positive correlation is desired, a zero correlation is
#' problematic, and a negative correlation requires immediate attention.
#'
#' The standard item-total correlation has an inherent flaw: the score of the
#' item being analyzed is included in the total score. This inclusion can
#' artificially inflate the correlation coefficient, as an item will always
#' correlate with itself. The *corrected* item-total correlation, or *item
#' discrimination*, addresses this issue by calculating the correlation between
#' the score on a single item and the sum of the scores of all other items on
#' the scale. This is done with `item_discrimination()`. The absolute value of
#' the item discrimination indices should be above `0.2`. An index between `0.2`
#' and `0.4` is considered as "fair", while a satisfactory index ranges from
#' `0.4` to `0.7`. Items with low discrimination indices are often ambiguously
#' worded and should be examined. Items with negative indices should be examined
#' to determine why a negative value was obtained (e.g. reversed answer
#' categories regarding positive and negative poles - in such cases, use
#' [`datawizard::reverse()`] to reverse-code items in advance).
#'
#' **Interpretation of the Corrected Item-Total Correlation Values:**
#'
#' | Corrected Item-Total Correlation Value | Interpretation | Action |
#' | :--- | :--- | :--- |
#' | **Above 0.40** | The item has a very good discrimination and is strongly related to the underlying construct. | Retain the item. |
#' | **0.30 to 0.39** | The item has good discrimination and contributes positively to the scale's internal consistency. | Retain the item. |
#' | **0.20 to 0.29** | The item has marginal discrimination. While not ideal, it may still be acceptable, especially in shorter scales or when measuring a very broad construct. | Consider revising the item for clarity or content. If other items have stronger correlations, this one might be a candidate for removal if the scale needs to be shortened. |
#' | **Below 0.20** | The item has poor discrimination. It does not correlate well with the rest of the scale and may be measuring something else. Its inclusion is likely to decrease the overall reliability (e.g., Cronbach's Alpha) of the scale. | Revise the item substantially or, more likely, remove it from the scale. |
#' | **Negative Value** | The item is negatively related to the rest of the scale. This is a serious issue. | The item must be revised or removed. Check for scoring errors (e.g., a reverse-keyed item that wasn't properly recoded). |
#'
#' `item_discrimination()` and `item_totalcor()` only differ in the default
#' value of the `corrected` argument. The former calculates the corrected
#' item-total correlations, while the latter calculates the item-total
#' correlations.
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
#' item_totalcor(x)
#' @export
item_discrimination <- function(x, standardize = FALSE, corrected = TRUE, verbose = TRUE) {
  # check param
  if (!is.matrix(x) && !is.data.frame(x)) {
    insight::format_alert("`x` needs to be a data frame or matrix.")
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
    if (corrected) {
      # compute item discrimination (corrected item-total correlation)
      score <- rowSums(x[, -i])
    } else {
      # compute item-total correlation
      score <- rowSums(x)
    }
    stats::cor(x[, i], score, use = "pairwise.complete.obs")
  }, numeric(1))

  # check for negative discrimination values. Tell user that item might need
  # to be reverse coded
  if (any(id < 0) && verbose) {
    insight::format_alert("Some of the values are negative. Maybe affected items need to be reverse-coded, e.g. using `datawizard::reverse()`.")
  }

  out <- data.frame(
    Item = df.names,
    Discrimination = id,
    stringsAsFactors = FALSE
  )

  # change label
  if (!corrected) {
    colnames(out)[2] <- "Item_Total_Correlation"
  }

  class(out) <- c("item_discrimination", "data.frame")
  out
}


#' @rdname item_discrimination
#' @export
item_totalcor <- function(x, standardize = FALSE, corrected = FALSE, verbose = TRUE) {
  # alias for item_discrimination, but corrected is FALSE by default
  item_discrimination(x, standardize = standardize, corrected = corrected, verbose = verbose)
}


# methods --------------------------------------

#' @export
print.item_discrimination <- function(x, ...) {
  cat(.print_item_discrimination(x, format = "text", ...))
  invisible(x)
}


#' @export
print_md.item_discrimination <- function(x, ...) {
  .print_item_discrimination(x, format = "markdown", ...)
}


#' @export
print_html.item_discrimination <- function(x, ...) {
  .print_item_discrimination(x, format = .check_format_backend(...), ...)
}


# helper ---------------------------------------

.print_item_discrimination <- function(x, format = "markdown", ...) {
  out <- insight::format_table(x, ...)
  # set correct caption
  if (colnames(out)[2] == "Discrimination") {
    caption <- "Item Discrimination"
  } else {
    colnames(out)[2] <- "Total Correlation"
    caption <- "Item-Total Correlation"
  }
  insight::export_table(out, caption = caption, format = format, ...)
}
