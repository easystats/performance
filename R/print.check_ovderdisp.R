#' @export
print.check_overdisp <- function(x, digits = 3, ...) {
  x$dispersion_ratio <- sprintf("%.*f", digits, x$dispersion_ratio)
  x$chisq_statistic <- sprintf("%.*f", digits, x$chisq_statistic)

  x$p_value <- pval <- round(x$p_value, digits = digits)
  if (x$p_value < .001) x$p_value <- "< 0.001"

  maxlen <- max(
    nchar(x$dispersion_ratio),
    nchar(x$chisq_statistic),
    nchar(x$p_value)
  )

  insight::print_color("# Overdispersion test\n\n", "blue")
  cat(sprintf("       dispersion ratio = %s\n", format(x$dispersion_ratio, justify = "right", width = maxlen)))
  cat(sprintf("  Pearson's Chi-Squared = %s\n", format(x$chisq_statistic, justify = "right", width = maxlen)))
  cat(sprintf("                p-value = %s\n\n", format(x$p_value, justify = "right", width = maxlen)))

  if (pval > 0.05)
    message("No overdispersion detected.")
  else
    message("Overdispersion detected.")
}
