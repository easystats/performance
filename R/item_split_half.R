#' @title Split-Half Reliability
#' @name item_split_half
#'
#' @description Compute various measures of internal consistencies
#'    for tests or item-scales of questionnaires.
#'
#' @param x A matrix or a data frame.
#' @param digits Amount of digits for returned values.
#'
#' @return A list with two elements: the split-half reliability \code{splithalf} and
#'      the Spearman-Brown corrected split-half reliability \code{spearmanbrown}.
#'
#' @details This function calculates the split-half reliability for items in
#'    \code{x}, including the Spearman-Brown adjustment. Splitting is done by
#'    selecting odd versus even columns in \code{x}. A value closer to 1
#'    indicates greater internal consistency.
#'
#' @references Spearman C. 1910. Correlation calculated from faulty data. British Journal of Psychology (3): 271-295. \doi{10.1111/j.2044-8295.1910.tb00206.x}
#'             \cr \cr
#'             Brown W. 1910. Some experimental results in the correlation of mental abilities. British Journal of Psychology (3): 296-322. \doi{10.1111/j.2044-8295.1910.tb00207.x}
#'
#' @examples
#' data(mtcars)
#' x <- mtcars[, c("cyl", "gear", "carb", "hp")]
#' item_split_half(x)
#' @export
item_split_half <- function(x, digits = 3) {
  # Calculating total score for even items
  score_e <- rowMeans(x[, c(TRUE, FALSE)], na.rm = TRUE)
  # Calculating total score for odd items
  score_o <- rowMeans(x[, c(FALSE, TRUE)], na.rm = TRUE)

  # Correlating scores from even and odd items
  shr <- stats::cor(score_e, score_o, use = "complete.obs")

  # Adjusting with the Spearman-Brown prophecy formula
  sb.shr <- (2 * shr) / (1 + shr)

  list(splithalf = shr, spearmanbrown = sb.shr)
}
