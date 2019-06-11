#' @title Model Performance
#' @name model_performance
#'
#' @description See the documentation for your object's class:
#' \link[=model_performance.merMod]{mixed models},
#' \link[=model_performance.stanreg]{Bayesian models} and
#' \link[=model_performance.lm]{all other models}.
#' \code{compare_performance()} computes indices of model performance for
#' different models at once and hence allows comparison of indices across models.
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
#' @details If all models are of the same class, \code{compare_performance()}
#'   returns an additional column named \code{BF}, which shows the Bayes factor
#'   (see \code{\link[bayestestR]{bayesfactor_models}}) for each model against
#'   the denominator model. The \emph{first} model is used as denominator model,
#'   and its Bayes factor is set to \code{NA} to indicate the reference model.
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
#' data(iris)
#' lm1 <- lm(Sepal.Length ~ Species, data = iris)
#' lm2 <- lm(Sepal.Length ~ Species + Petal.Length, data = iris)
#' lm3 <- lm(Sepal.Length ~ Species * Petal.Length, data = iris)
#' compare_performance(lm1, lm2, lm3)
#'
#' @export
model_performance <- function(model, ...) {
  UseMethod("model_performance")
}


#' @importFrom insight is_model all_models_equal
#' @importFrom bayestestR bayesfactor_models
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

  m <- mapply(function(.x, .y) {
    dat <- model_performance(.x, metrics = metrics)
    cbind(data.frame(Model = as.character(.y), Type = class(.x)[1], stringsAsFactors = FALSE), dat)
  }, objects, object_names, SIMPLIFY = FALSE)


  # check for identical model class, for bayesfactor
  BFs <- NULL
  if (insight::all_models_equal(...)) {
    BFs <- tryCatch({
      bayestestR::bayesfactor_models(..., denominator = 1, verbose = FALSE)
    },
    error = function(e) { NULL })
  }

  dfs <- Reduce(function(x, y) merge(x, y, all = TRUE, sort = FALSE), m)

  if (!is.null(BFs)) {
    dfs$BF <- BFs$BF
    dfs$BF[dfs$Model == object_names[1]] <- NA
  }

  # dfs[order(sapply(object_names, as.character), dfs$Model), ]
  dfs
}
