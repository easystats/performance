#' @title Describe Properties of Item Scales
#' @name check_itemscale
#'
#' @description Compute various measures of internal consistencies
#'   applied to (sub)scales, which items were extracted using
#'   \code{\link[parameters]{principal_components}}.
#'
#' @param x An object of class \code{parameters_pca}, as returned by \code{\link[parameters]{principal_components}}.
#'
#' @return A list of data frames, with related measures of internal consistencies of each subscale.
#'
#' @details \code{check_itemscale()} calculates various measures of internal
#'   consistencies, such as Cronbach's alpha, item difficulty or discrimination etc.
#'   on subscales which were built from several items. Subscales are retrieved from
#'   the results of \code{\link[parameters]{principal_components}}, i.e. based on
#'   how many components were extracted from the PCA, \code{check_itemscale()}
#'   retrieves those variables that belong to a component and calculates the above
#'   mentioned measures.
#'
#' @note \itemize{
#'   \item \emph{Item difficulty} should range between 0.2 and 0.8. Ideal value is \code{p+(1-p)/2} (which mostly is between 0.5 and 0.8). See \code{\link{item_difficulty}} for details.
#'   \item For \emph{item discrimination}, acceptable values are 0.20 or higher; the closer to 1.00 the better. See \code{\link{item_reliability}} for more details.
#'   \item In case the total \emph{Cronbach's alpha} value is below the acceptable cut-off of 0.7 (mostly if an index has few items), the \emph{mean inter-item-correlation} is an alternative measure to indicate acceptability. Satisfactory range lies between 0.2 and 0.4. See also \code{\link{item_intercor}}.
#' }
#'
#' @references \itemize{
#'   \item Briggs SR, Cheek JM (1986) The role of factor analysis in the development and evaluation of personality scales. Journal of Personality, 54(1), 106-148. doi: 10.1111/j.1467-6494.1986.tb00391.x
#'   \item Trochim WMK (2008) Types of Reliability. (\href{http://www.socialresearchmethods.net/kb/reltypes.php}{web})
#' }
#'
#' @examples
#' \donttest{
#' library(parameters)
#' # data generation from '?prcomp', slightly modified
#' C <- chol(S <- toeplitz(.9^(0:15)))
#' set.seed(17)
#' X <- matrix(rnorm(16000), 100, 16)
#' Z <- X %*% C
#' pca <- principal_components(as.data.frame(Z), rotation = "varimax")
#' pca
#' check_itemscale(pca)
#' }
#' @importFrom stats sd
#' @export
check_itemscale <- function(x) {
  if (!inherits(x, "parameters_pca")) {
    stop("'x' must be an object of class 'parameters_pca', as returned by 'parameters::principal_components()'.")
  }
  if (!requireNamespace("parameters", quietly = TRUE)) {
    stop("Package 'parameters' required for this function to work. Please install it.", call. = FALSE)
  }

  data_set <- attributes(x)$data_set
  subscales <- parameters::closest_component(x)

  out <- lapply(sort(unique(subscales)), function(.subscale) {
    columns <- names(subscales)[subscales == .subscale]
    items <- data_set[columns]
    reliability <- item_reliability(items)

    .item_discr <- reliability$item_discrimination
    if (is.null(.item_discr)) .item_discr <- NA
    .item_alpha <- reliability$alpha_if_deleted
    if (is.null(.item_alpha)) .item_alpha <- NA

    s_out <- data.frame(
      Item = columns,
      Missings = sapply(items, function(i) sum(is.na(i)) / nrow(items)),
      Mean = sapply(items, mean, na.rm = TRUE),
      SD = sapply(items, stats::sd, na.rm = TRUE),
      Skewness = sapply(items, parameters::skewness),
      "Difficulty" = item_difficulty(items)$difficulty,
      "Discrimination" = .item_discr,
      "alpha if deleted" = .item_alpha,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    attr(s_out, "item_intercorrelation") <- item_intercor(items)
    attr(s_out, "cronbachs_alpha") <- cronbachs_alpha(items)

    s_out
  })

  class(out) <- unique(c("check_itemscale", class(out)))
  out
}
