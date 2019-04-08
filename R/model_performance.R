#' Model Performance
#'
#' See the documentation for your object's class:
#' \itemize{
#'  \item{\link[=model_performance.lm]{lm}}
#'  \item{\link[=model_performance.glm]{glm}}
#'  \item{\link[=model_performance.merMod]{mixed models}}
#'  \item{\link[=model_performance.stanreg]{Bayesian models}}
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

temp_model_performance <- function(..., metrics = "all") {
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
