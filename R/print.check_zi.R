#' @importFrom insight print_color
#' @export
print.check_zi <- function(x, ...) {
  insight::print_color("\n# Check for zero-inflation\n\n", "blue")
  cat(sprintf("   Observed zeros: %i\n", x$observed.zeros))
  cat(sprintf("  Predicted zeros: %i\n", x$predicted.zeros))
  cat(sprintf("            Ratio: %.2f\n\n", x$ratio))

  lower <- 1 - x$tolerance
  upper <- 1 + x$tolerance

  if (x$ratio < lower)
    message("Model is underfitting zeros (probable zero-inflation).")
  else if (x$ratio > upper)
    message("Model is overfitting zeros.")
  else
    message("Model seems ok, ratio of observed and predicted zeros is within the tolerance range.")
}
