#' @importFrom insight is_model_supported all_models_equal get_response
#' @importFrom bayestestR bayesfactor_models
#' @inheritParams model_performance.lm
#' @rdname model_performance
#' @export
compare_performance <- function(..., metrics = "all", verbose = TRUE) {
  objects <- list(...)
  object_names <- match.call(expand.dots = FALSE)$`...`

  supported_models <- sapply(objects, function(i) insight::is_model_supported(i) | inherits(i, "lavaan"))

  if (!all(supported_models)) {
    warning(sprintf("Following objects are not supported: %s", paste0(object_names[!supported_models], collapse = ", ")))
    objects <- objects[supported_models]
    object_names <- object_names[supported_models]
  }

  m <- mapply(function(.x, .y) {
    dat <- model_performance(.x, metrics = metrics)
    cbind(data.frame(Model = as.character(.y), Type = class(.x)[1], stringsAsFactors = FALSE), dat)
  }, objects, object_names, SIMPLIFY = FALSE)


  # check for identical model class, for bayesfactor
  BFs <- tryCatch(
    {
      bayestestR::bayesfactor_models(..., denominator = 1, verbose = FALSE)
    },
    error = function(e) {
      NULL
    }
  )

  dfs <- Reduce(function(x, y) merge(x, y, all = TRUE, sort = FALSE), m)

  if (!is.null(BFs)) {
    dfs$BF <- BFs$BF
    dfs$BF[dfs$Model == object_names[1]] <- NA
  }

  # check if all models were fit from same data
  resps <- lapply(objects, insight::get_response)
  if (!all(sapply(resps[-1], function(x) identical(x, resps[[1]]))) && verbose) {
    warning("When comparing models, please note that probably not all models were fit from same data.", call. = FALSE)
  }

  # dfs[order(sapply(object_names, as.character), dfs$Model), ]
  dfs
}
