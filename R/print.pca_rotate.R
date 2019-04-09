#' @export
print.pca_rotate <- function(x, cutoff = 0.1, digits = 3, ...) {

  insight::print_color(sprintf("# Rotated loadings from Principal Component Analysis (%s-rotation)\n\n", attr(x, "rotation", exact = TRUE)), "blue")

  xs <- attr(x, "variance", exact = TRUE)
  x <- round(x, digits = digits)

  x <- as.data.frame(apply(x, MARGIN = c(1, 2), function(.y) {
    if (abs(.y) < cutoff)
      ""
    else
      as.character(.y)
  }), stringsAsFactors = FALSE)

  xs <- as.data.frame(t(as.data.frame(round(xs, digits = digits))))

  colnames(xs) <- sprintf("PC%i", 1:ncol(xs))
  rownames(xs) <- c("Proportion variance", "Cumulative variance", "Proportion explained", "Cumulative explained")

  print(x, quote = FALSE, ...)
  insight::print_color("\n(Explained) Variance\n", "cyan")
  print(xs, ...)
}



