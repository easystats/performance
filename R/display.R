#' @title Print tables in different output formats
#' @name display.performance_model
#'
#' @description Prints tables (i.e. data frame) in different output formats.
#'
#' @param object,x An object returned by one of the package's function, for
#'   example [`model_performance()`], [`compare_performance()`], or
#'   [`check_itemscale()`].
#' @param format String, indicating the output format. Can be `"markdown"`
#'   `"html"`, or `"tt"`. `format = "tt"` creates a `tinytable` object, which is
#'   either printed as markdown or HTML table, depending on the environment. See
#'   [`insight::export_table()`] for details.
#' @param layout Table layout (can be either `"horizontal"` or `"vertical"`).
#' @param digits Number of decimal places.
#' @param caption Table caption as string. If `NULL`, no table caption is printed.
#' @param ... Currently not used.
#'
#' @section Global Options to Customize Output when Printing:
#'
#' - `easystats_display_format`: `options(easystats_display_format = <value>)`
#'   will set the default format for the `display()` methods. Can be one of
#'   `"markdown"`, `"html"`, or `"tt"`.
#'
#' @return A character vector. If `format = "markdown"`, the return value
#'   will be a character vector in markdown-table format.
#'
#' @details `display()` is useful when the table-output from functions, which is
#'   usually printed as formatted text-table to console, should be formatted for
#'   pretty table-rendering in markdown documents, or if knitted from rmarkdown
#'   to PDF or Word files. See
#'   [vignette](https://easystats.github.io/parameters/articles/model_parameters_formatting.html)
#'   for examples.
#'
#' @examples
#' model <- lm(mpg ~ wt + cyl, data = mtcars)
#' mp <- model_performance(model)
#' display(mp)
#' @export
display.performance_model <- function(
  object,
  format = "markdown",
  digits = 2,
  caption = NULL,
  ...
) {
  format <- .display_default_format(format)
  if (format %in% c("html", "tt")) {
    print_html(
      x = object,
      digits = digits,
      caption = caption,
      backend = ifelse(format == "tt", "tt", "html"),
      ...
    )
  } else {
    print_md(x = object, digits = digits, caption = caption, ...)
  }
}

#' @export
display.compare_performance <- display.performance_model

#' @export
display.check_itemscale <- display.performance_model

#' @export
display.item_discrimination <- display.performance_model

#' @export
display.item_reliability <- display.performance_model

#' @export
display.item_omega <- display.performance_model

#' @export
display.item_difficulty <- display.performance_model

.display_default_format <- function(format) {
  format <- getOption("easystats_display_format", format)
  insight::validate_argument(format, c("markdown", "html", "md", "tt"))
}
