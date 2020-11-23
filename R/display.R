#' @title Print tables in different output formats
#' @name display.performance_model
#'
#' @description Prints tables (i.e. data frame) in different output formats.
#'   \code{print_md()} is a alias for \code{display(format = "markdown")}.
#'
#' @param object,x An object returned by \code{\link[=model_performance]{model_performance()}}
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
#' @details \code{display()} is useful when the table-output from functions,
#'   which is usually printed as formatted text-table to console, should
#'   be formatted for pretty table-rendering in markdown documents, or if
#'   knitted from rmarkdown to PDF or Word files. See
#'   \href{https://easystats.github.io/parameters/articles/model_parameters_formatting.html}{vignette}
#'   for examples.
#'
#' @examples
#' model <- lm(mpg ~ wt + cyl, data = mtcars)
#' mp <- model_performance(model)
#' display(mp)
#' @export
display.performance_model <- function(object, format = "markdown", digits = 2, ...) {
  print_md(x = object, digits = digits, ...)
}


#' @export
display.compare_performance <- display.performance_model


#' @export
display.check_itemscale <- display.performance_model




# Reexports models ------------------------

#' @importFrom insight display
#' @export
insight::display
