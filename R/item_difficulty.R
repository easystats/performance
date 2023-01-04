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
#'   columns in `x`.
#' @return A data frame with three columns: The name(s) of the item(s), the item
#'      difficulties for each item, and the ideal item difficulty.
#'
#' @details This function calculates the item difficulty, which should
#'    range between 0.2 and 0.8. Lower values are a signal for
#'    more difficult items, while higher values close to one
#'    are a sign for easier items. The ideal value for item difficulty
#'    is `p + (1 - p) / 2`, where `p = 1 / max(x)`. In most
#'    cases, the ideal item difficulty lies between 0.5 and 0.8.
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
  } else if (!is.numeric(maximum_value)) {
    insight::format_error("`maximum_value` must be a numeric value, indicating the maximum value of an item.")
  }

  d <- sapply(x, function(.x) {
    .x <- .x[!is.na(.x)]
    round(sum(.x) / (maximum_value * length(.x)), 2)
  })

  # ideal item item_difficulty
  fun.diff.ideal <- function(.x) {
    p <- 1 / maximum_value
    round(p + (1 - p) / 2, 2)
  }

  di <- apply(x, 2, fun.diff.ideal)

  structure(
    class = c("item_difficulty", "data.frame"),
    data.frame(
      item = colnames(x),
      difficulty = d,
      ideal = di,
      stringsAsFactors = FALSE
    )
  )
}



# methods --------------------------------------

#' @export
print.item_difficulty <- function(x, ...) {
  spaces <- max(nchar(x$item))

  insight::print_color("# Item Difficulty\n\n", "blue")
  insight::print_color(sprintf("  %*s  ideal\n", spaces + 10, "difficulty"), "red")

  for (i in seq_along(x$item)) {
    cat(sprintf("  %*s      %.2f   %.2f\n", spaces, x$item[i], x$difficulty[i], x$ideal[i]))
  }
  invisible(x)
}
