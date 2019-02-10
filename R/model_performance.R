#' Model Performance
#'
#' See the documentation for your object's class:
#' \itemize{
#'  \item{\link[=model_performance.lm]{lm}}
#'  \item{\link[=model_performance.glm]{glm}}
#'  \item{\link[=model_performance.stanreg]{Bayesian lm}}
#'  }
#'
#'
#' @param model Statistical model.
#' @param ... Arguments passed to or from other methods.
#'
#'
#' @export
model_performance <- function(model, ...) {
  UseMethod("model_performance")
}
