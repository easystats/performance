#' @title Model Performance
#' @name model_performance
#'
#' @description See the documentation for your object's class: \link[=model_performance.lm]{lm},
#' \link[=model_performance.glm]{glm}, \link[=model_performance.merMod]{mixed models}
#' and \link[=model_performance.stanreg]{Bayesian models}. \code{compare_performance()}
#' computes indices of model performance for different models.
#'
#' @param model Statistical model.
#' @param metrics Can be \code{"all"} or a character vector of metrics to be computed.
#'   See related documentation of object's class for details.
#' @param ... Arguments passed to or from other methods, resp. for
#'   \code{compare_performance()}, one or multiple model objects (also of
#'   different classes).
#'
#' @return For \code{model_performance()}, a data frame (with one row) and one
#'   column per "index" (see \code{metrics}). For \code{compare_performance()},
#'   the same data frame with one row per model.
#'
#' @examples
#' library(lme4)
#'
#' m1 <- lm(mpg ~ wt + cyl, data = mtcars)
#' model_performance(m1)
#'
#' m2 <- glm(vs ~ wt + mpg, data = mtcars, family = "binomial")
#' m3 <- lmer(Petal.Length ~ Sepal.Length + (1 | Species), data = iris)
#' compare_performance(m1, m2, m3)
#'
#' @export
model_performance <- function(model, ...) {
  UseMethod("model_performance")
}


#' @importFrom insight is_model
#' @rdname model_performance
#' @export
compare_performance <- function(..., metrics = "all") {
  objects <- list(...)
  object_names <- match.call(expand.dots = FALSE)$`...`

  supported_models <- sapply(objects, insight::is_model)

  if (!all(supported_models)) {
    warning(sprintf("Following objects are not supported: %s", paste0(object_names[!supported_models], collapse = ", ")))
    objects <- objects[supported_models]
    object_names <- object_names[supported_models]
  }

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
