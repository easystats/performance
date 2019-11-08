#' @export
plot.performance_roc <- function(x, ...) {
  if (!requireNamespace("see", quietly = TRUE)) {
    stop("Package 'see' needed to plot ROC-curves. Please install it.")
  }
  NextMethod()
}



#' @export
plot.check_outliers <- function(x, ...) {
  if (!requireNamespace("see", quietly = TRUE)) {
    stop("Package 'see' needed to plot outliers. Please install it.")
  }
  NextMethod()
}



#' @export
plot.check_collinearity <- function(x, ...) {
  if (!requireNamespace("see", quietly = TRUE)) {
    stop("Package 'see' needed to plot collinearity-check. Please install it.")
  }
  NextMethod()
}



#' @export
plot.check_distribution <- function(x, ...) {
  if (!requireNamespace("see", quietly = TRUE)) {
    stop("Package 'see' needed to plot predicted distributions. Please install it.")
  }
  NextMethod()
}



#' @export
plot.check_distribution_numeric <- function(x, ...) {
  if (!requireNamespace("see", quietly = TRUE)) {
    stop("Package 'see' needed to plot predicted distributions. Please install it.")
  }
  NextMethod()
}



#' @export
plot.check_normality <- function(x, ...) {
  if (!requireNamespace("see", quietly = TRUE)) {
    stop("Package 'see' needed for residual plots. Please install it.")
  }
  NextMethod()
}


#' @export
plot.check_heteroscedasticity <- function(x, ...) {
  if (!requireNamespace("see", quietly = TRUE)) {
    stop("Package 'see' needed for residual plots. Please install it.")
  }
  NextMethod()
}


#' @export
plot.check_homogeneity <- function(x, ...) {
  if (!requireNamespace("see", quietly = TRUE)) {
    stop("Package 'see' needed for homogeneity plots. Please install it.")
  }
  NextMethod()
}
