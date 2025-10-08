#' @title Reliability Test for Items or Scales
#' @name item_reliability
#'
#' @description Compute various measures of internal consistencies for tests or
#' item-scales of questionnaires.
#'
#' @param x A matrix or a data frame.
#' @param standardize Logical, if `TRUE`, the data frame's vectors will be
#' standardized. Recommended when the variables have different measures /
#' scales.
#' @param digits Amount of digits for returned values.
#' @param verbose Toggle warnings and messages.
#'
#' @return A data frame with the item-total correlations (column
#' `Item_Total_Correlation`), corrected item-total correlations (*item
#' discrimination*, column `Discrimination`) and Cronbach's Alpha (if item
#' deleted, column `Alpha_if_deleted`) for each item of the scale, or `NULL` if
#' data frame had too less columns.
#'
#' @inherit check_itemscale note references
#'
#' @details
#' This function calculates the item-total correlations, item discriminations
#' (corrected item-total correlations for each item of `x` with the remaining
#' items) and the Cronbach's alpha for each item, if it was deleted from the
#' scale. The absolute value of the item discrimination indices should be above
#' 0.2. An index between 0.2 and 0.4 is considered as "fair", while an index
#' above 0.4 (or below -0.4) is "good". The range of satisfactory values is from
#' 0.4 to 0.7. Items with low discrimination indices are often ambiguously
#' worded and should be examined. Items with negative indices should be examined
#' to determine why a negative value was obtained (e.g. reversed answer
#' categories regarding positive and negative poles).
#'
#' See [`check_itemscale()`] and [`item_discrimination()`] for more details on
#' the interpretation of the results.
#'
#' @examples
#' data(mtcars)
#' x <- mtcars[, c("cyl", "gear", "carb", "hp")]
#' item_reliability(x)
#' @export
item_reliability <- function(x, standardize = FALSE, digits = 3, verbose = TRUE) {
  # check param
  if (!is.matrix(x) && !is.data.frame(x)) {
    if (verbose) {
      insight::format_alert("`x` needs to be a data frame or matrix.")
    }
    return(NULL)
  }

  # remove missings, so correlation works
  x <- stats::na.omit(x)

  # remember item (column) names for return value
  # return value gets column names of initial data frame
  df.names <- colnames(x)
  out <- NULL

  # check for minimum amount of columns can't be less than 3, because the
  # reliability test checks for Cronbach's alpha if a specific item is deleted.
  # If data frame has only two columns and one is deleted, Cronbach's alpha
  # cannot be calculated.

  if (ncol(x) > 2) {
    # Check whether items should be scaled. Needed,
    # when items have different measures / scales
    if (standardize) {
      x <- .std(x)
    }

    # calculate cronbach-if-deleted
    cronbachDeleted <- vapply(
      seq_len(ncol(x)),
      function(i) cronbachs_alpha(x[, -i]),
      numeric(1L)
    )

    # calculate corrected total-item correlation
    totalCorrCorrected <- item_discrimination(
      x,
      standardize = FALSE,
      verbose = verbose
    )

    # calculate total-item correlation
    totalCorr <- item_totalcor(
      x,
      standardize = FALSE,
      verbose = FALSE
    )

    out <- data.frame(
      Item = df.names,
      Alpha_if_deleted = round(cronbachDeleted, digits),
      Item_Total_Correlation = round(totalCorr$Item_Total_Correlation, digits),
      Discrimination = round(totalCorrCorrected$Discrimination, digits),
      stringsAsFactors = FALSE
    )

    attr(out, "item_intercorrelation") <- item_intercor(x)
    attr(out, "cronbachs_alpha") <- cronbachs_alpha(x)
    class(out) <- c("item_reliability", "data.frame")
  } else {
    insight::format_warning(
      "Data frame needs at least three columns for reliability-test."
    )
  }

  out
}


# methods --------------------------------------

#' @export
print.item_reliability <- function(x, ...) {
  out <- insight::format_table(x, ...)
  # format column names
  colnames(out) <- c("Item", "Alpha if deleted", "Total Correlation", "Discrimination")

  # add attributes for table caption and footer
  attr(out, "table_caption") <- c("# Item Reliability", "blue")
  attr(out, "table_footer") <- c(
    sprintf(
      "\nMean inter-item-correlation = %.3f  Cronbach's alpha = %.3f",
      attributes(out)$item_intercorrelation,
      attributes(out)$cronbachs_alpha
    ),
    "yellow"
  )

  cat(insight::export_table(out, ...))
  invisible(x)
}


#' @export
print_md.item_reliability <- function(x, ...) {
  .print_item_reliability(x, format = "markdown", ...)
}


#' @export
print_html.item_reliability <- function(x, ...) {
  .print_item_reliability(x, format = .check_format_backend(...), ...)
}


# helper ---------------------------------------

.print_item_reliability <- function(x, format = "markdown", ...) {
  out <- insight::format_table(x, ...)
  # format column names
  colnames(out) <- c("Item", "Alpha if deleted", "Total Correlation", "Discrimination")

  # add attributes for table caption and footer
  caption <- "Item Reliability"
  footer <- sprintf(
    "Mean inter-item-correlation = %.3f  Cronbach's alpha = %.3f",
    attributes(out)$item_intercorrelation,
    attributes(out)$cronbachs_alpha
  )

  insight::export_table(
    out,
    caption = caption,
    footer = footer,
    format = format,
    missing = "<NA>",
    align = "firstleft",
    zap_small = TRUE
  )
}
