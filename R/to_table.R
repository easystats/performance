#' @title Export tables into different output formats
#' @name to_table.performance_model
#'
#' @description Export tables (i.e. data frame) into different output formats.
#'   \code{table_to_markdown()} is a alias for \code{to_table(format = "markdown")}.
#'
#' @param x An object returned by \code{\link[=model_performance]{model_performance()}}
#'   or \code{\link[=compare_performance]{compare_performance()}}.
#'   or its summary.
#' @param format String, indicating the output format. Currently, only
#'   \code{"markdown"} is supported.
#' @param digits Number of decimal places.
#' @param ... Currently not used.
#'
#' @return A character vector. If \code{format = "markdown"}, the return value
#'   will be a character vector in markdown-table format.
#'
#' @details \code{to_table()} is useful when the table-output from functions,
#'   which is usually printed as formatted text-table to console, should
#'   be formatted for pretty table-rendering in markdown documents, or if
#'   knitted from rmarkdown to PDF or Word files.
#'
#' @examples
#' model <- lm(mpg ~ wt + cyl, data = mtcars)
#' mp <- model_performance(model)
#' to_table(mp)
#' @importFrom insight format_table format_p
#' @export
to_table.performance_model <- function(x, format = "markdown", digits = 2, ...) {
  table_caption <- "Indices of model performance"

  # format p-values for meta-analysis
  if ("p_CochransQ" %in% colnames(x)) {
    x$p_CochransQ <- insight::format_p(x$p_CochransQ)
  }
  if ("p_Omnibus" %in% colnames(x)) {
    x$p_Omnibus <- insight::format_p(x$p_Omnibus)
  }

  x <- .format_df_columns(x)
  x <- .format_freq_stats(x)

  insight::format_table(x = x, digits = digits, format = format, caption = table_caption, align = "firstleft")
}


#' @export
to_table.compare_performance <- function(x, format = "markdown", digits = 2, ...) {
  table_caption <- "Comparison of Model Performance Indices"

  if ("Performance_Score" %in% colnames(x)) {
    footer <- sprintf("Model %s (of class %s) performed best with an overall performance score of %s.\n", x$Model[1], x$Type[1], x$Performance_Score[1])
  } else {
    footer <- NULL
  }

  # if we have ranking, add score and remove incomplete indices in print
  if ("Performance_Score" %in% colnames(x)) {
    x$Performance_Score <- sprintf("%.2f%%", 100 * x$Performance_Score)
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

  x <- .format_df_columns(x)
  x <- .format_freq_stats(x)

  insight::format_table(x = x, digits = digits, format = format, caption = table_caption, footer = footer, align = "firstleft")
}




# Reexports models ------------------------

#' @importFrom insight to_table
#' @export
insight::to_table
