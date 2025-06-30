#' @title Difficulty of Questionnaire Items
#' @name item_difficulty
#'
#' @description Compute various measures of internal consistencies
#'    for tests or item-scales of questionnaires.
#'
#' @param x Depending on the function, `x` may be a `matrix` as
#'    returned by the `cor()`-function, or a data frame
#'    with items (e.g. from a test or questionnaire).
#' @param maximum_value Numeric value, indicating the maximum value of an item.
#'   If `NULL` (default), the maximum is taken from the maximum value of all
#'   columns in `x` (assuming that the maximum value at least appears once in
#'   the data). If `NA`, each item's maximum value is taken as maximum. If the
#'   required maximum value is not present in the data, specify the theoreritcal
#'   maximum using `maximum_value`.
#' @return A data frame with three columns: The name(s) of the item(s), the item
#'      difficulties for each item, and the ideal item difficulty.
#'
#' @details _Item difficutly_ of an item is defined as the quotient of the sum
#'   actually achieved for this item of all and the maximum achievable score.
#'   This function calculates the item difficulty, which should range between
#'   0.2 and 0.8. Lower values are a signal for more difficult items, while
#'   higher values close to one are a sign for easier items. The ideal value
#'   for item difficulty is `p + (1 - p) / 2`, where `p = 1 / max(x)`. In most
#'   cases, the ideal item difficulty lies between 0.5 and 0.8.
#'
#' @references
#' - Bortz, J., and Döring, N. (2006). Quantitative Methoden der Datenerhebung.
#'   In J. Bortz and N. Döring, Forschungsmethoden und Evaluation. Springer:
#'   Berlin, Heidelberg: 137–293
#' - Kelava A, Moosbrugger H (2020). Deskriptivstatistische Itemanalyse und
#'   Testwertbestimmung. In: Moosbrugger H,  Kelava A, editors. Testtheorie und
#'   Fragebogenkonstruktion. Berlin, Heidelberg: Springer, 143–158
#'
#' @examples
#' data(mtcars)
#' x <- mtcars[, c("cyl", "gear", "carb", "hp")]
#' item_difficulty(x)
#' @export
item_difficulty <- function(x, maximum_value = NULL) {
  # find general maximum of scale
  if (is.null(maximum_value)) {
    maximum_value <- suppressWarnings(max(vapply(x, max, numeric(1L), na.rm = TRUE)))
  } else if (!is.na(maximum_value) && !is.numeric(maximum_value)) {
    insight::format_error("`maximum_value` must be a numeric value, indicating the maximum value of an item.")
  }

  d <- vapply(x, function(.x) {
    # general maximum value, or per-item maximum value?
    if (is.na(maximum_value)) {
      max_val <- max(.x, na.rm = TRUE)
    } else {
      max_val <- maximum_value
    }
    .x <- .x[!is.na(.x)]
    round(sum(.x) / (max_val * length(.x)), 2)
  }, numeric(1))

  # ideal item item_difficulty
  fun.diff.ideal <- function(.x) {
    # general maximum value, or per-item maximum value?
    if (is.na(maximum_value)) {
      max_val <- max(.x, na.rm = TRUE)
    } else {
      max_val <- maximum_value
    }
    p <- 1 / max_val
    round(p + (1 - p) / 2, 2)
  }

  di <- vapply(x, fun.diff.ideal, numeric(1))

  structure(
    class = c("item_difficulty", "data.frame"),
    data.frame(
      Item = colnames(x),
      Difficulty = d,
      Ideal = di,
      stringsAsFactors = FALSE
    )
  )
}


# methods --------------------------------------

#' @export
print.item_difficulty <- function(x, ...) {
  out <- insight::format_table(x, ...)
  cat(insight::export_table(out, caption = c("Item Difficulty", "blue"), ...))
  invisible(x)
}


#' @export
print_md.item_difficulty <- function(x, ...) {
  out <- insight::format_table(x, ...)
  insight::export_table(out, caption = "Item Difficulty", format = "markdown", ...)
}


#' @export
print_html.item_difficulty <- function(x, ...) {
  out <- insight::format_table(x, ...)
  insight::export_table(out, caption = "Item Difficulty", format = "html", ...)
}
