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
    x[weighted_ics] <- lapply(x[weighted_ics], function(wt) {
      out <- insight::format_bf(wt, name = NULL, exact = TRUE)
      # a model without this IC has no weight, not a weight of 1
      out[is.na(wt)] <- NA
      out
    })
  }

  if ("BF" %in% colnames(x)) {
    x$BF[is.na(x$BF)] <- 1
    # x$BF <- insight::format_bf(x$BF, exact = TRUE)
  }
  out <- insight::format_table(x, digits = digits, ...)

  # drop the empty "()" that format_table() adds for a missing weight
  weight_columns <- endsWith(colnames(out), "(weights)")
  out[weight_columns] <- lapply(out[weight_columns], function(i) {
    trimws(gsub("()", "", i, fixed = TRUE))
  })
  out
}


#' @export
format.performance_model <- function(x, digits = 2, ...) {
  # format p-values for meta-analysis
  insight::format_table(x, digits = digits, ...)
}
