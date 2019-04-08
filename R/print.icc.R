#' @importFrom insight print_color
#' @export
print.icc <- function(x, digits = 3, ...) {
  insight::print_color("# Intraclass Correlation Coefficient\n\n", "blue")

  out <- paste0(c(
    sprintf("     Adjusted ICC: %.*f", digits, x$ICC_adjusted),
    sprintf("  Conditional ICC: %.*f", digits, x$ICC_conditional)),
    collapse = "\n"
  )

  cat(out)
  cat("\n")
}