#' @title Model Performance
#' @name model_performance
#'
#' @description See the documentation for your object's class:
#'
#'  - [Frequentist Regressions][model_performance.lm]
#'  - [Instrumental Variables Regressions][model_performance.ivreg]
#'  - [Mixed models][model_performance.merMod]
#'  - [Bayesian models][model_performance.stanreg]
#'  - [CFA / SEM lavaan models][model_performance.lavaan]
#'  - [Meta-analysis models][model_performance.rma]
#'
#' @seealso [`compare_performance()`][compare_performance] to compare performance of many different models.
#'
#' @param model Statistical model.
#' @param ... Arguments passed to or from other methods, resp. for
#'   `compare_performance()`, one or multiple model objects (also of
#'   different classes).
#'
#' @return A data frame (with one row) and one column per "index" (see `metrics`).
#'
#' @details `model_performance()` correctly detects transformed response and
#' returns the "corrected" AIC and BIC value on the original scale. To get back
#' to the original scale, the likelihood of the model is multiplied by the
#' Jacobian/derivative of the transformation.
#'
#' @examples
#' model <- lm(mpg ~ wt + cyl, data = mtcars)
#' model_performance(model)
#'
#' model <- glm(vs ~ wt + mpg, data = mtcars, family = "binomial")
#' model_performance(model)
#' @export
model_performance <- function(model, ...) {
  UseMethod("model_performance")
}


#' @rdname model_performance
#' @export
performance <- model_performance


# methods --------------------------------

#' @export
print.performance_model <- function(x, digits = 3, layout = "horizontal", ...) {
  layout <- match.arg(layout, choices = c("horizontal", "vertical"))
  formatted_table <- format(x = x, digits = digits, format = "text", ...)

  # switch to vertical layout
  if (layout == "vertical") {
    formatted_table <- datawizard::rownames_as_column(as.data.frame(t(formatted_table)), "Metric")
    colnames(formatted_table)[2] <- "Value"
  }

  cat(
    insight::export_table(
      x = formatted_table,
      digits = digits,
      format = "text",
      caption = c("# Indices of model performance", "blue"),
      ...
    )
  )

  invisible(x)
}
