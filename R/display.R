#' @title Print tables in different output formats
#' @name display.performance_model
#'
#' @description Prints tables (i.e. data frame) in different output formats.
#'   `print_md()` is a alias for `display(format = "markdown")`.
#'
#' @param object,x An object returned by [`model_performance()`] or
#'   or [`compare_performance()`].
#'   or its summary.
#' @param format String, indicating the output format. Currently, only
#'   `"markdown"` is supported.
#' @param layout Table layout (can be either `"horizontal"` or `"vertical"`).
#' @param digits Number of decimal places.
#' @param caption Table caption as string. If `NULL`, no table caption is printed.
#' @param ... Currently not used.
#'
#' @return A character vector. If `format = "markdown"`, the return value
#'   will be a character vector in markdown-table format.
#'
#' @details `display()` is useful when the table-output from functions,
#'   which is usually printed as formatted text-table to console, should
#'   be formatted for pretty table-rendering in markdown documents, or if
#'   knitted from rmarkdown to PDF or Word files. See
#'   [vignette](https://easystats.github.io/parameters/articles/model_parameters_formatting.html)
#'   for examples.
#'
#' @examples
#' model <- lm(mpg ~ wt + cyl, data = mtcars)
#' mp <- model_performance(model)
#' display(mp)
#' @export
display.performance_model <- function(object, format = "markdown", digits = 2, caption = NULL, ...) {
  if (identical(format, "html")) {
    print_html(x = object, digits = digits, caption = caption, ...)
  } else {
    print_md(x = object, digits = digits, caption = caption, ...)
  }
}


#' @export
display.compare_performance <- display.performance_model


#' @export
display.check_itemscale <- display.performance_model
