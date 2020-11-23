#' @importFrom insight format_value format_p format_bf
#' @export
format.compare_performance <- function(x, digits = 2, format = NULL, ...) {
  # if we have ranking, add score and remove incomplete indices in print
  if ("Performance_Score" %in% colnames(x)) {
    x$Performance_Score <- insight::format_value(x$Performance_Score, as_percent = TRUE)
    x <- x[!sapply(x, anyNA)]
  }

  # format p-values for meta-analysis
  if ("p_CochransQ" %in% colnames(x)) {
    x$p_CochransQ <- insight::format_p(x$p_CochransQ)
  }
  if ("p_Omnibus" %in% colnames(x)) {
    x$p_Omnibus <- insight::format_p(x$p_Omnibus)
  }
  if ("BF" %in% colnames(x)) {
    x$BF[is.na(x$BF)] <- 1
    x$BF <- insight::format_bf(x$BF)
  }

  as.data.frame(x)
}


#' @export
format.performance_model <- function(x, digits = 2, format = NULL, ...) {
  # format p-values for meta-analysis
  if ("p_CochransQ" %in% colnames(x)) {
    x$p_CochransQ <- insight::format_p(x$p_CochransQ)
  }
  if ("p_Omnibus" %in% colnames(x)) {
    x$p_Omnibus <- insight::format_p(x$p_Omnibus)
  }

  x <- .format_df_columns(x)
  x <- .format_freq_stats(x)

  as.data.frame(x)
}
