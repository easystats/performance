#' @title Rotate loading matrix of principal component analysis
#' @name pca_rotate
#' @description ...
#'
#' @param x A data frame or a \code{\link[stats]{prcomp}} object.
#' @param n_comp Number of components to extract. If \code{rotation = "varmiax"}
#'    and \code{n_comp = NULL}, number of components is based on the Kaiser-criteria.
#' @param rotation Rotation of the factor loadings. May be one of
#'    \code{"varimax", "quartimax", "promax", "oblimin", "simplimax", "cluster"}
#'    or \code{"none"}.
#'
#' @return A rotated loadings matrix, as data frame. Details on the variance
#'   components are saved as attributes.
#'
#' @details The \code{print()}-method for \code{pca_rotate()} has a
#'    \code{cutoff}-argument, which is a scalar between 0 and 1, indicating
#'    which (absolute) values from the loadings should be blank in the
#'    output. By default, all loadings between -.1 and .1 are not shown.
#'
#' @examples
#' data(iris)
#' pca_rotate(iris[, 1:4], n_comp = 2)
#'
#' pr <- pca_rotate(iris[, 1:4], n_comp = 2)
#'
#' # show all
#' print(pr, cutoff = .001)
#'
#' # show only some
#' print(pr, cutoff = .5)
#'
#' @importFrom stats varimax
#' @export
pca_rotate <- function(x, n_comp = NULL, rotation = c("varimax", "quartimax", "promax", "oblimin", "simplimax", "cluster", "none")) {

  rotation <- match.arg(rotation)

  if (!inherits(x, c("prcomp", "data.frame")))
    stop("`x` must be of class `prcomp` or a data frame.", call. = F)

  if (!inherits(x, "data.frame") && rotation != "varimax")
    stop(sprintf("`x` must be a data frame for `%s`-rotation.", rotation), call. = F)

  if (rotation != "varimax" && !requireNamespace("psych", quietly = TRUE))
    stop(sprintf("Package `psych` required for `%s`-rotation.", rotation), call. = F)


  # rotate loadings

  if (rotation != "varimax")
    tmp <- psych::principal(r = x, n_compactors = n_comp, rotate = rotation)
  else {
    if (!inherits(x, "pca")) x <- pca(x)

    loadings <- attr(x, "loadings", exact = TRUE)
    if (is.null(n_comp)) n_comp <- attr(x, "kaiser", exact = TRUE)

    if (n_comp < 2) {
      stop("Can't rotate loadings, too few components extracted.", call. = F)
    }

    tmp <- stats::varimax(loadings[, seq_len(n_comp)])
  }


  # tweak column names and class attributes

  tmp <- as.data.frame(unclass(tmp$loadings))
  colnames(tmp) <- sprintf("PC%i", 1:ncol(tmp))
  class(tmp) <- c("pca_rotate", "data.frame")


  # add explained proportions and proportional and cumulative variance

  .prop.var <- colSums(tmp^2) / nrow(tmp)
  .cum.var <- cumsum(.prop.var)
  .prop.exp <- .prop.var / sum(.prop.var)
  .cum.exp <- cumsum(.prop.exp)

  attr(tmp, "rotation") <- rotation

  attr(tmp, "variance") <- data.frame(
    prop.var = .prop.var,
    cum.var = .cum.var,
    prop.exp = .prop.exp,
    cum.exp = .cum.exp,
    stringsAsFactors = FALSE
  )

  tmp
}
