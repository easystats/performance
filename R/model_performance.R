#' @title Model Performance
#' @name model_performance
#'
#' @description See the documentation for your object's class:
#' \itemize{
#'   \item \link[=model_performance.lm]{Frequentist Regressions}
#'   \item \link[=model_performance.merMod]{Mixed models}
#'   \item \link[=model_performance.stanreg]{Bayesian models}
#'   \item \link[=model_performance.lavaan]{CFA / SEM lavaan models}
#' }
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
#' @details If all models were fit from the same data, \code{compare_performance()}
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
