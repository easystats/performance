#' @title Mean Inter-Item-Correlation
#' @name item_intercor
#'
#' @description Compute various measures of internal consistencies
#'    for tests or item-scales of questionnaires.
#'
#' @param x A matrix as returned by the \code{\link{cor}}-function,
#'    or a data frame with items (e.g. from a test or questionnaire).
#' @param method Correlation computation method. May be one of
#'    \code{"spearman"} (default), \code{"pearson"} or \code{"kendall"}.
#'    You may use initial letter only.
#'
#' @return The mean inter-item-correlation value for \code{x}.
#'
#' @details This function calculates a mean inter-item-correlation, i.e.
#'    a correlation matrix of \code{x} will be computed (unless
#'    \code{x} is already a matrix as returned by the \code{cor()}-function)
#'    and the mean of the sum of all item's correlation values is returned.
#'    Requires either a data frame or a computed \code{cor()}-object.
#'    \cr \cr
#'    \dQuote{Ideally, the average inter-item correlation for a set of
#'    items should be between .20 and .40, suggesting that while the
#'    items are reasonably homogenous, they do contain sufficiently
#'    unique variance so as to not be isomorphic with each other.
#'    When values are lower than .20, then the items may not be
#'    representative of the same content domain. If values are higher than
#'    .40, the items may be only capturing a small bandwidth of the construct.}
#'    \cite{(Piedmont 2014)}
#'
#' @references Piedmont RL. 2014. Inter-item Correlations. In: Michalos AC (eds) Encyclopedia of Quality of Life and Well-Being Research. Dordrecht: Springer, 3303-3304. \doi{10.1007/978-94-007-0753-5_1493}
#'
#' @examples
#' data(mtcars)
#' x <- mtcars[, c("cyl", "gear", "carb", "hp")]
#' item_intercor(x)
#' @importFrom stats cor na.omit
#' @export
item_intercor <- function(x, method = c("pearson", "spearman", "kendall")) {
  # Check parameter
  method <- match.arg(method)

  # Mean-interitem-corelation
  if (inherits(x, "matrix")) {
    corr <- x
  } else {
    x <- stats::na.omit(x)
    corr <- stats::cor(x, method = method)
  }

  mean(corr[lower.tri(corr)])
}
