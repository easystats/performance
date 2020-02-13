#' @title Model Performance
#' @name model_performance
#'
#' @description See the documentation for your object's class:
#' \itemize{
#'   \item \link[=model_performance.lm]{Frequentist Regressions}
#'   \item \link[=model_performance.merMod]{Mixed models}
#'   \item \link[=model_performance.stanreg]{Bayesian models}
#'   \item \link[=model_performance.lavaan]{CFA / SEM lavaan models}
#'   \item \link[=model_performance.rma]{Meta-analysis models}
#' }
#'
#' @seealso \code{\link[=compare_performance]{compare_performance()}} to compare performance of many different models.
#'
#' @param model Statistical model.
#' @param ... Arguments passed to or from other methods, resp. for
#'   \code{compare_performance()}, one or multiple model objects (also of
#'   different classes).
#'
#' @return A data frame (with one row) and one column per "index" (see \code{metrics}).
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
