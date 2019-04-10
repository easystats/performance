#' @importFrom insight print_color
#' @export
print.r2_bayes <- function(x, digits = 3, ...) {
  insight::print_color("# Bayesian R2 with Standard Error\n\n", "blue")

  out <- sprintf("  Conditional R2: %.*f [%.*f]", digits, x$R2_Bayes, digits, attr(x, "std.error")[["R2_Bayes"]])

  if ("R2_Bayes_marginal" %in% names(x)) {
    out <- paste0(c(out,  sprintf("     Marginal R2: %.*f [%.*f]", digits, x$R2_Bayes_marginal, digits, attr(x, "std.error")[["R2_Bayes_marginal"]])), collapse = "\n")
  }

  cat(out)
  cat("\n")
}