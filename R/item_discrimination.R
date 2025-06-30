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
#' This function calculates the item discriminations (corrected item-total
#' correlations for each item of `x` with the remaining items) for each item
#' of a scale. The absolute value of the item discrimination indices should be
#' above `0.2`. An index between `0.2` and `0.4` is considered as "fair", while a
#' satisfactory index ranges from `0.4` to `0.7`. Items with low discrimination
#' indices are often ambiguously worded and should be examined. Items with
#' negative indices should be examined to determine why a negative value was
#' obtained (e.g. reversed answer categories regarding positive and negative
#' poles - in such cases, use [`datawizard::reverse()`] to reverse-code items
#' in advance).
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
  out <- insight::format_table(x, ...)
  # set correct caption
  if (colnames(out)[2] == "Discrimination") {
    caption <- c("Item Discrimination", "blue")
  } else {
    colnames(out)[2] <- "Total Correlation"
    caption <- c("Item-Total Correlation", "blue")
  }
  cat(insight::export_table(out, caption = caption, ...))
  invisible(x)
}


#' @export
print_md.item_discrimination <- function(x, ...) {
  out <- insight::format_table(x, ...)
  # set correct caption
  if (colnames(out)[2] == "Discrimination") {
    caption <- "Item Discrimination"
  } else {
    colnames(out)[2] <- "Total Correlation"
    caption <- "Item-Total Correlation"
  }
  insight::export_table(out, caption = caption, format = "markdown", ...)
}


#' @export
print_html.item_discrimination <- function(x, ...) {
  out <- insight::format_table(x, ...)
  # set correct caption
  if (colnames(out)[2] == "Discrimination") {
    caption <- "Item Discrimination"
  } else {
    colnames(out)[2] <- "Total Correlation"
    caption <- "Item-Total Correlation"
  }
  insight::export_table(out, caption = caption, format = "html", ...)
}


#' @export
display.item_discrimination <- display.performance_model
