#' @title Model Performance
#' @name model_performance
#'
#' @description See the documentation for your object's class: \link[=model_performance.lm]{lm},
#' \link[=model_performance.glm]{glm}, \link[=model_performance.merMod]{mixed models}
#' and \link[=model_performance.stanreg]{Bayesian models}. \code{compare_performance()}
#' computes indices of model performance for different models.
#'
#' @param model Statistical model.
#' @param ... Arguments passed to or from other methods, resp. for \code{compare_performance()}, one or multiple model objects (also of different classes).
#'
#'
#' @export
model_performance <- function(model, ...) {
  UseMethod("model_performance")
}


#' @rdname model_performance
#' @export
compare_performance <- function(..., metrics = "all") {
  objects <- list(...)
  object_names <- match.call(expand.dots = FALSE)$`...`

  m <- lapply(objects, function(.x) {
    dat <- model_performance(.x, metrics = metrics)
    cbind(data.frame(class = class(.x)[1], stringsAsFactors = FALSE), dat)
  })

  dfs <- Reduce(function(x, y) merge(x, y, all = TRUE), m)

  cbind(
    data.frame(
      name = unlist(lapply(object_names, as.character)),
      stringsAsFactors = FALSE
    ),
    dfs
  )
}
