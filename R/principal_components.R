#' @title Principal Components Analysis
#' @name principal_components
#'
#' @description This function performs a principal component analysis and
#'   returns the loadings (of the unrotated matrix) as data frame, or
#'   returns a rotated matrix of the loadings (if \code{rotation} is not
#'   \code{NULL}).
#'
#' @param x A data frame or a \code{\link[stats]{prcomp}}-object.
#' @param rotation Rotation of the factor loadings. May be one of
#'    \code{"varimax", "quartimax", "promax", "oblimin", "simplimax", "cluster"}
#'    or \code{"none"}. If \code{rotatoin = NULL}, loadings for the principal
#'    components from the unrotated matrix are returned.
#' @param n_comp Number of components to extract. If \code{rotation = "varmiax"}
#'    and \code{n_comp = NULL}, number of components is based on the Kaiser-criteria.
#'
#' @return If \code{rotation = NULL}, a data frame with all loadings of principal
#'   components. Else, a rotated loadings matrix, as data frame. Details on the
#'   variance components are saved as attributes.
#'
#' @details The \code{print()}-method has a \code{cutoff}-argument, which is a
#'   scalar between 0 and 1, indicating which (absolute) values from the
#'   \emph{rotated} loadings (i.e. when \code{rotation} is \emph{not} \code{NULL})
#'   should be blank in the output. By default, all loadings between -.1 and .1
#'   are not shown.
#'
#'
#' @examples
#' data(iris)
#' principal_components(iris[, 1:4])
#'
#' data(iris)
#' principal_components(iris[, 1:4], rotation = "varimax", n_comp = 2)
#'
#' pr <- principal_components(iris[, 1:4], rotation = "varimax", n_comp = 2)
#'
#' # show all
#' print(pr, cutoff = .001)
#'
#' # show only some
#' print(pr, cutoff = .5)
#'
#' @importFrom stats prcomp na.omit varimax
#' @export
principal_components <- function(x, rotation = NULL, n_comp = NULL) {
  if (is.null(rotation))
    .pca(x)
  else
    .pca_rotate(x, rotation, n_comp)
}



.pca <- function(x) {

  if (!inherits(x, c("prcomp", "data.frame")))
    stop("`x` must be of class `prcomp` or a data frame.", call. = F)


  # if x is a df, run prcomp

  if (inherits(x, "data.frame"))
    x <- stats::prcomp(stats::na.omit(x), retx = TRUE, center = TRUE, scale. = TRUE)


  # get tidy summary of prcomp object

  .comp <- sprintf("PC%i", seq_len(length(x$sdev)))
  .std.dev <- x$sdev
  .eigen <- .std.dev^2
  .prop.var <- .eigen / sum(.eigen)
  .cum.var <- cumsum(.prop.var)

  tmp <- data.frame(
    comp = .comp,
    std.dev = .std.dev,
    eigen = .eigen,
    prop.var = .prop.var,
    cum.var = .cum.var,
    stringsAsFactors = FALSE
  )

  # add in_compormation on Kaiser criteria and loadings
  attr.kaiser <- which(tmp$eigen < 1)[1] - 1
  attr.loadings <- x$rotation %*% diag(x$sdev)


  # rotate df for proper output
  cnames <- tmp[[1]]
  tmp <- tmp[, -1]

  tmp <- as.data.frame(t(as.data.frame(tmp)))
  colnames(tmp) <- cnames
  rownames(tmp) <- NULL

  attr(tmp, "kaiser") <- attr.kaiser
  attr(tmp, "loadings") <- attr.loadings

  # add class-attribute for printing
  class(tmp) <- c("perf_pca", class(tmp))

  tmp
}


.pca_rotate <- function(x, rotation, n_comp) {

  if (!(rotation %in% c("varimax", "quartimax", "promax", "oblimin", "simplimax", "cluster", "none"))) {
    stop("`rotation` must be one of \"varimax\", \"quartimax\", \"promax\", \"oblimin\", \"simplimax\", \"cluster\" or \"none\".")
  }

  if (!inherits(x, c("prcomp", "data.frame")))
    stop("`x` must be of class `prcomp` or a data frame.", call. = F)

  if (!inherits(x, "data.frame") && rotation != "varimax")
    stop(sprintf("`x` must be a data frame for `%s`-rotation.", rotation), call. = F)


  if (!inherits(x, "pca")) {
    pca_data <- .pca(x)
  } else {
    pca_data <- x
  }

  if (is.null(n_comp)) n_comp <- attr(pca_data, "kaiser", exact = TRUE)


  # rotate loadings

  if (rotation != "varimax") {
    if (!requireNamespace("psych", quietly = TRUE)) {
      stop(sprintf("Package `psych` required for `%s`-rotation.", rotation), call. = F)
    }
    tmp <- psych::principal(r = x, nfactors = n_comp, rotate = rotation)
  } else {
    loadings <- attr(pca_data, "loadings", exact = TRUE)
    if (n_comp < 2) {
      stop("Can't rotate loadings, too few components extracted.", call. = F)
    }
    tmp <- stats::varimax(loadings[, seq_len(n_comp)])
  }


  # tweak column names and class attributes

  tmp <- as.data.frame(unclass(tmp$loadings))
  colnames(tmp) <- sprintf("PC%i", 1:ncol(tmp))
  class(tmp) <- c("perf_pca_rotate", "data.frame")


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
