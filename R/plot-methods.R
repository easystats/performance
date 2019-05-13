#' @export
plot.performance_roc <- function(x, ...) {
  if (!requireNamespace("see", quietly = TRUE)) {
    stop("Package 'see' needed to plot ROC-curves. Please install it.")
  }
  NextMethod()
}
