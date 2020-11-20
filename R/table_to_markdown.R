#' @rdname to_table.performance_model
#' @export
table_to_markdown.performance_model <- function(x, digits = 2, ...) {
  to_table(x = x, digits = digits, format = "markdown", ...)
}

#' @export
table_to_markdown.compare_performance <- table_to_markdown.performance_model

#' @export
table_to_markdown.check_itemscale <- table_to_markdown.performance_model


# Reexports models ------------------------

#' @importFrom insight table_to_markdown
#' @export
insight::table_to_markdown
