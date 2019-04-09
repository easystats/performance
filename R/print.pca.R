#' @export
print.pca <- function (x, digits = 3, ...) {
  x <- as.data.frame(round(x, digits = digits))
  rownames(x) <- c("Standard deviation", "Eigenvalue", "Proportion variance", "Cumulative variance")
  print(x, ...)
}
