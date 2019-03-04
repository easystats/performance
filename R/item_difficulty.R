#' @title Difficulty of Questionnaire Items
#' @name item_difficulty
#'
#' @description Compute various measures of internal consistencies
#'    for tests or item-scales of questionnaires.
#'
#' @param x Depending on the function, \code{x} may be a \code{matrix} as
#'    returned by the \code{\link{cor}}-function, or a data frame
#'    with items (e.g. from a test or questionnaire).
#'
#' @return A list with three elements: The name(s) of the item(s), the item
#'      difficulties for each item, and the ideal item difficulty.
#'
#' @details This function calculates the item difficutly, which should
#'    range between 0.2 and 0.8. Lower values are a signal for
#'    more difficult items, while higher values close to one
#'    are a sign for easier items. The ideal value for item difficulty
#'    is \code{p + (1 - p) / 2}, where \code{p = 1 / max(x)}. In most
#'    cases, the ideal item difficulty lies between 0.5 and 0.8.
#'
#' @examples
#' data(mtcars)
#' x <- mtcars[, c("cyl", "gear", "carb", "hp")]
#' item_difficulty(x)
#'
#'
#' @importFrom stats na.omit
#' @export
item_difficulty <- function(x) {
  d <- apply(x, 2, function(.x) {
    .x <- stats::na.omit(.x)
    round(sum(.x) / (max(.x) * length(.x)), 2)
  })

  # ideal item item_difficulty
  fun.diff.ideal <- function(.x) {
    p <- 1 / max(.x, na.rm = T)
    round(p + (1 - p) / 2, 2)
  }

  di <- apply(x, 2, fun.diff.ideal)

  list(
    items = colnames(x),
    item_difficulty = d,
    ideal.difficulty = di
  )
}
