#' @title Summary of principal component analysis
#' @name pca
#'
#' @description This function performs a principal component analysis and
#'   returns the loadings as data frame.
#'
#' @param x A data frame or a \code{\link[stats]{prcomp}}-object.
#'
#' @return A data frame with all loadings of principal components.
#'
#' @examples
#' data(iris)
#' pca(iris[, 1:4])
#'
#' @importFrom stats prcomp na.omit
#' @export
pca <- function(x) {

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
  class(tmp) <- c("pca", class(tmp))

  tmp
}

