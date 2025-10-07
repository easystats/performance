#' @rdname display.performance_model
#' @export
print_md.performance_model <- function(x,
                                       digits = 2,
                                       caption = "Indices of model performance",
                                       layout = "horizontal",
                                       ...) {
  layout <- insight::validate_argument(layout, c("horizontal", "vertical"))
  formatted_table <- format(
    x = x,
    digits = digits,
    format = "markdown",
    ...
  )

  # switch to vertical layout
  if (layout == "vertical") {
    formatted_table <- datawizard::rownames_as_column(as.data.frame(t(formatted_table)), "Metric")
    colnames(formatted_table)[2] <- "Value"
  }

  insight::export_table(
    x = formatted_table,
    digits = digits,
    format = "markdown",
    caption = caption,
    align = "firstleft",
    ...
  )
}


#' @rdname display.performance_model
#' @export
print_md.compare_performance <- function(x,
                                         digits = 2,
                                         caption = "Comparison of Model Performance Indices",
                                         layout = "horizontal",
                                         ...) {
  layout <- insight::validate_argument(layout, c("horizontal", "vertical"))
  .print_md_compare_performance(x, digits = digits, caption = caption, layout = layout, format = "markdown", ...)
}


#' @export
print_html.compare_performance <- function(x,
                                           digits = 2,
                                           caption = "Comparison of Model Performance Indices",
                                           layout = "horizontal",
                                           ...) {
  layout <- insight::validate_argument(layout, c("horizontal", "vertical"))
  .print_md_compare_performance(
    x,
    digits = digits,
    caption = caption,
    layout = layout,
    format = .check_format_backend(...),
    ...
  )
}


# helper ------------------------------------

.print_md_compare_performance <- function(x,
                                          digits = 2,
                                          caption = "Comparison of Model Performance Indices",
                                          layout = "horizontal",
                                          format = "markdown",
                                          ...) {
  layout <- insight::validate_argument(layout, c("horizontal", "vertical"))
  formatted_table <- format(x = x, digits = digits, format = format, ...)

  if ("Performance_Score" %in% colnames(x)) {
    footer <- sprintf(
      "Model %s (of class %s) performed best with an overall performance score of %s.",
      formatted_table$Model[1],
      formatted_table$Type[1],
      formatted_table$Performance_Score[1]
    )
  } else {
    footer <- NULL
  }

  # switch to vertical layout
  if (layout == "vertical") {
    formatted_table <- datawizard::rownames_as_column(as.data.frame(t(formatted_table)), "Metric")
    formatted_table <- datawizard::row_to_colnames(formatted_table)
    colnames(formatted_table)[1] <- "Metric"
  }

  insight::export_table(
    x = formatted_table,
    digits = digits,
    format = format,
    caption = caption,
    footer = footer,
    align = "firstleft"
  )
}
