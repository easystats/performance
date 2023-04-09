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
#' @param standardize Standardize the dataframe before clustering (default).
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
check_clusterstructure <- function(x,
                                   standardize = TRUE,
                                   distance = "euclidean",
                                   ...) {
  if (standardize) {
    x <- as.data.frame(scale(x))
  }

  H <- .clusterstructure_hopkins(x, distance = distance)
  if (H < 0.5) {
    text <- paste0(
      "The dataset is suitable for clustering (Hopkins' H = ",
      insight::format_value(H),
      ").\n"
    )
    color <- "green"
  } else {
    text <- paste0(
      "The dataset is not suitable for clustering (Hopkins' H = ",
      insight::format_value(H),
      ").\n"
    )
    color <- "red"
  }

  out <- list(
    H = H,
    dissimilarity_matrix = .clusterstructure_dm(x, distance = distance, method = "ward.D2")
  )

  attr(out, "text") <- text
  attr(out, "color") <- color
  attr(out, "title") <- "Clustering tendency"
  class(out) <- c("see_check_clusterstructure", "check_clusterstructure", "easystats_check", class(out))
  out
}



#' @export
plot.check_clusterstructure <- function(x, ...) {
  # Can be reimplemented with ggplot in see
  stats::heatmap(
    x$dissimilarity_matrix,
    Rowv = NA, Colv = NA,
    labRow = FALSE, labCol = FALSE,
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
  # This is based on the hopkins() function from the clustertend package
  if (is.data.frame(x)) {
    x <- as.matrix(x)
  }

  n <- nrow(x) - 1

  c <- apply(x, 2, min) # minimum value per column
  d <- apply(x, 2, max)
  p <- matrix(0, ncol = ncol(x), nrow = n) # n vectors of space
  for (i in seq_len(ncol(x))) {
    p[, i] <- stats::runif(n, min = c[i], max = d[i])
  }
  k <- round(stats::runif(n, 1, nrow(x)))
  q <- as.matrix(x[k, ])
  distp <- rep(0, nrow(x))
  # distq=rep(0,nrow(x)-1)
  distq <- 0
  minp <- rep(0, n)
  minq <- rep(0, n)
  for (i in 1:n) {
    distp[1] <- stats::dist(rbind(p[i, ], x[1, ]), method = distance)
    minqi <- stats::dist(rbind(q[i, ], x[1, ]), method = distance)
    for (j in 2:nrow(x)) {
      distp[j] <- stats::dist(rbind(p[i, ], x[j, ]), method = distance)
      error <- q[i, ] - x[j, ]
      if (sum(abs(error)) != 0) {
        # distq[j]<-stats::dist(rbind(q[i,],x[j,]))
        distq <- stats::dist(rbind(q[i, ], x[j, ]), method = distance)
        if (distq < minqi) {
          minqi <- distq
        }
      }
    }
    minp[i] <- min(distp)
    # minq[i]<-apply(distq,1,min)
    minq[i] <- minqi
  }
  sum(minq) / (sum(minp) + sum(minq))
}
