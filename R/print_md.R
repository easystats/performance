#' @importFrom insight export_table format_p
#' @rdname display.performance_model
#' @export
print_md.performance_model <- function(x, digits = 2, ...) {
  formatted_table <- format(x = x, digits = digits, format = "markdown", ...)
  insight::export_table(x = formatted_table, digits = digits, format = "markdown", caption = "Indices of model performance", align = "firstleft", ...)
}


#' @export
print_md.compare_performance <- function(x, digits = 2, ...) {
  table_caption <- "Comparison of Model Performance Indices"
  formatted_table <- format(x = x, digits = digits, format = "markdown", ...)
  if ("Performance_Score" %in% colnames(x)) {
    footer <- sprintf("Model %s (of class %s) performed best with an overall performance score of %s.", formatted_table$Model[1], formatted_table$Type[1], formatted_table$Performance_Score[1])
  } else {
    footer <- NULL
  }
  insight::export_table(x = formatted_table, digits = digits, format = "markdown", caption = table_caption, footer = footer, align = "firstleft")
}


#' @export
print_html.compare_performance <- function(x, digits = 2, ...) {
  table_caption <- "Comparison of Model Performance Indices"
  formatted_table <- format(x = x, digits = digits, format = "html", ...)
  if ("Performance_Score" %in% colnames(x)) {
    footer <- sprintf("Model %s (of class %s) performed best with an overall performance score of %s.", formatted_table$Model[1], formatted_table$Type[1], formatted_table$Performance_Score[1])
  } else {
    footer <- NULL
  }
  insight::export_table(x = formatted_table, digits = digits, format = "html", caption = table_caption, footer = footer, align = "firstleft")
}


#' @export
print_md.check_itemscale <- function(x, digits = 2, ...) {
  insight::export_table(
    lapply(1:length(x), function(i) {
      out <- x[[i]]
      attr(out, "caption") <- sprintf("Component %i", i)
      attr(out, "footer") <- sprintf(
        "Mean inter-item-correlation = %.3f  Cronbach's alpha = %.3f",
        attributes(out)$item_intercorrelation,
        attributes(out)$cronbachs_alpha
      )
      out
    }),
    digits = digits,
    format = "markdown",
    missing = "<NA>",
    align = "firstleft",
    zap_small = TRUE
  )
}


# Reexports models ------------------------

#' @importFrom insight print_md
#' @export
insight::print_md


#' @importFrom insight print_html
#' @export
insight::print_html
