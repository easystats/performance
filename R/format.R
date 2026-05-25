#' @export
format.compare_performance <- function(x, digits = 2, ...) {
  # if we have ranking, add score and remove incomplete indices in print
  if ("Performance_Score" %in% colnames(x)) {
    x$Performance_Score <- insight::format_value(x$Performance_Score, as_percent = TRUE)
    x <- x[!sapply(x, anyNA)]
  }

  # format weighted ICs
  weighted_ics <- endsWith(colnames(x), "_wt")
  if (any(weighted_ics)) {
    x[weighted_ics] <- lapply(
      x[weighted_ics],
      insight::format_bf,
      name = NULL,
      exact = TRUE
    )
  }

  if ("BF" %in% colnames(x)) {
    x$BF[is.na(x$BF)] <- 1
    # x$BF <- insight::format_bf(x$BF, exact = TRUE)
  }
  insight::format_table(x, digits = digits, ...)
}


#' @export
format.performance_model <- function(x, digits = 2, ...) {
  # format p-values for meta-analysis
  insight::format_table(x, digits = digits, ...)
}
