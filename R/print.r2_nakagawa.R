#' @importFrom insight print_color
#' @export
print.r2_nakagawa <- function(x, digits = 3, ...) {
  insight::print_color("# R2 for mixed models\n\n", "blue")

  out <- paste0(c(
    sprintf("  Conditional R2: %.*f", digits, x$R2_conditional),
    sprintf("     Marginal R2: %.*f", digits, x$R2_marginal)),
    collapse = "\n"
  )

  cat(out)
  cat("\n")
}