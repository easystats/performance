temp_model_performance <- function(model, ...) {
  objects <- list(...)
  object_names <- match.call(expand.dots = FALSE)$`...`

  m <- lapply(objects, NextMethod)
  dfs <- Reduce(function(x, y) merge(x, y, all = TRUE), m)

  cbind(
    data.frame(
      names = unlist(lapply(object_names, as.character)),
      class = unlist(sapply(objects, function(x) class(x)[1])),
      stringsAsFactors = FALSE
    ),
    dfs
  )
}
