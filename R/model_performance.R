#' @title Model Performance
#' @name model_performance
#'
#' @description See the documentation for your object's class:
#' \itemize{
#'   \item [Frequentist Regressions][model_performance.lm]
#'   \item [Instrumental Variables Regressions][model_performance.ivreg]
#'   \item [Mixed models][model_performance.merMod]
#'   \item [Bayesian models][model_performance.stanreg]
#'   \item [CFA / SEM lavaan models][model_performance.lavaan]
#'   \item [Meta-analysis models][model_performance.rma]
#' }
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
print.performance_model <- function(x, digits = 3, ...) {
  formatted_table <- format(x = x, digits = digits, format = "text", ...)
  cat(insight::export_table(x = formatted_table, digits = digits, format = "text", caption = c("# Indices of model performance", "blue"), ...))
  invisible(x)
}
