#' Check suitability of data for clustering
#'
#' This checks whether the data is appropriate for clustering using the Hopkins'
#' H statistic of given data. If the value of Hopkins statistic is close to 0
#' (below 0.5), then we can reject the null hypothesis and conclude that the
#' dataset is significantly clusterable. A value for H lower than 0.25 indicates
#' a clustering tendency at the `90%` confidence level. The visual assessment of
#' cluster tendency (VAT) approach (Bezdek and Hathaway, 2002) consists in
#' investigating the heatmap of the ordered dissimilarity matrix. Following
#' this, one can potentially detect the clustering tendency by counting the
#' number of square shaped blocks along the diagonal.
#'
#' @param x A data frame.
#' @param standardize Standardize the data frame before clustering (default).
#' @param distance Distance method used. Other methods than "euclidean"
#'   (default) are exploratory in the context of clustering tendency. See
#'   [stats::dist()] for list of available methods.
#' @param ... Arguments passed to or from other methods.
#'
#' @examples
#' \donttest{
#' library(performance)
#' check_clusterstructure(iris[, 1:4])
#' plot(check_clusterstructure(iris[, 1:4]))
#' }
#' @return The H statistic (numeric)
#'
#' @seealso [`check_kmo()`], [`check_sphericity_bartlett()`] and
#' [`check_factorstructure()`].
#'
#' @references
#' - Lawson, R. G., & Jurs, P. C. (1990). New index for clustering
#'   tendency and its application to chemical problems. Journal of chemical
#'   information and computer sciences, 30(1), 36-41.
#'
#' - Bezdek, J. C., & Hathaway, R. J. (2002, May). VAT: A tool for visual
#'   assessment of (cluster) tendency. In Proceedings of the 2002 International
#'   Joint Conference on Neural Networks. IJCNN02 (3), 2225-2230. IEEE.
#' @export
check_clusterstructure <- function(x, standardize = TRUE, distance = "euclidean", ...) {
  if (standardize) {
    x <- as.data.frame(scale(x))
  }

  H <- .clusterstructure_hopkins(x, distance = distance)
  if (H < 0.5) {
    res_text <- paste0(
      "The dataset is suitable for clustering (Hopkins' H = ",
      insight::format_value(H),
      ").\n"
    )
    color <- "green"
  } else {
    res_text <- paste0(
      "The dataset is not suitable for clustering (Hopkins' H = ",
      insight::format_value(H),
      ").\n"
    )
    color <- "red"
  }

  out <- list(
    H = H,
    dissimilarity_matrix = .clusterstructure_dm(
      x,
      distance = distance,
      method = "ward.D2"
    )
  )

  attr(out, "text") <- res_text
  attr(out, "color") <- color
  attr(out, "title") <- "Clustering tendency"
  class(out) <- c(
    "see_check_clusterstructure",
    "check_clusterstructure",
    "easystats_check",
    class(out)
  )
  out
}


#' @export
plot.check_clusterstructure <- function(x, ...) {
  # Can be reimplemented with ggplot in see
  stats::heatmap(
    x$dissimilarity_matrix,
    Rowv = NA,
    Colv = NA,
    labRow = FALSE,
    labCol = FALSE,
    col = grDevices::colorRampPalette(c("#2196F3", "#FAFAFA", "#E91E63"))(100)
  )
}


#' @keywords internal
.clusterstructure_dm <- function(x, distance = "euclidean", method = "ward.D2") {
  d <- stats::dist(x, method = distance)
  hc <- stats::hclust(d, method = method)
  as.matrix(d)[hc$order, hc$order]
}


#' @keywords internal
.clusterstructure_hopkins <- function(x, distance = "euclidean") {
  # Based on Hopkins' statistic (Lawson & Jurs, 1990)
  if (is.data.frame(x)) {
    x <- as.matrix(x)
  }

  nr <- nrow(x)
  n <- nr - 1L

  cc <- apply(x, 2, min) # minimum value per column
  d <- apply(x, 2, max)
  p <- matrix(0, ncol = ncol(x), nrow = n) # n vectors of space
  for (i in seq_len(ncol(x))) {
    p[, i] <- stats::runif(n, min = cc[i], max = d[i])
  }
  k <- sample.int(nr, n)

  dist_all <- as.matrix(stats::dist(rbind(p, x), method = distance))
  dist_px <- dist_all[seq_len(n), (n + 1L):(n + nr), drop = FALSE]
  dist_xx <- dist_all[(n + 1L):(n + nr), (n + 1L):(n + nr), drop = FALSE]
  dist_xx[dist_xx == 0] <- Inf

  minp <- apply(dist_px, 1, min)
  minq <- apply(dist_xx[k, , drop = FALSE], 1, min)
  minq[!is.finite(minq)] <- 0

  sum(minq) / (sum(minp) + sum(minq))
}
