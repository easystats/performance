#' @title Simple ROC curve
#' @name performance_roc
#'
#' @description This function calculates a simple ROC curves of x/y coordinates
#' based on response and predictions of a binomial model.
#'
#' @param x A numeric vector, representing the outcome (0/1), or a model with
#' binomial outcome.
#' @param predictions If \code{x} is numeric, a numeric vector of same length
#' as \code{x}, representing the actual predicted values.
#' @param new_data If \code{x} is a model, a data frame that is passed to
#' \code{predict()} as \code{newdata}-argument. If \code{NULL}, the ROC for
#' the full model is calculated.
#' @param ... One or more models with binomial outcome. In this case,
#' \code{new_data} is ignored.
#'
#' @return A data frame with three columns, the x/y-coordinate pairs for the ROC
#' curve (\code{Sensivity} and \code{Specifity}), and a column with the model
#' name.
#'
#' @examples
#' library(bayestestR)
#' data(iris)
#'
#' set.seed(123)
#' iris$y <- rbinom(nrow(iris), size = 1, .3)
#' folds <- sample(nrow(iris), size = nrow(iris) / 8, replace = FALSE)
#' test_data <- iris[folds, ]
#' train_data <- iris[-folds, ]
#'
#' model <- glm(y ~ Sepal.Length + Sepal.Width, data = train_data, family = "binomial")
#' performance_roc(model, new_data = test_data)
#'
#' roc <- performance_roc(model, new_data = test_data)
#' area_under_curve(roc$Sensivity, roc$Specifity)
#'
#' m1 <- glm(y ~ Sepal.Length + Sepal.Width, data = iris, family = "binomial")
#' m2 <- glm(y ~ Sepal.Length + Petal.Width, data = iris, family = "binomial")
#' m3 <- glm(y ~ Sepal.Length + Species, data = iris, family = "binomial")
#' performance_roc(m1, m2, m3)
#' @importFrom stats predict
#' @importFrom insight find_response get_data
#' @export
performance_roc <- function(x, ..., predictions, new_data) {
  dots <- list(...)

  object_names <- c(
    .safe_deparse(substitute(x)),
    sapply(match.call(expand.dots = FALSE)$`...`, .safe_deparse)
  )

  if (is.numeric(x) && !missing(predictions) && !is.null(predictions)) {
    .performance_roc_numeric(x, predictions)
  } else if (inherits(x, "glm") && length(dots) == 0) {
    if (missing(new_data)) new_data <- NULL
    .performance_roc_model(x, new_data)
  } else if (length(dots) > 0) {
    .performance_roc_models(list(x, ...), names = object_names)
  }
}



.performance_roc_numeric <- function(x, predictions) {
  if (length(x) != length(predictions)) {
    stop("'x' and ' predictions' must be of same length.", call. = FALSE)
  }

  x <- x[order(predictions, decreasing = TRUE)]

  res <- data.frame(
    Sensivity = c(0, cumsum(x) / sum(x), 1),
    Specifity = c(0, cumsum(!x) / sum(!x), 1)
  )

  class(res) <- c("performance_roc", "see_performance_roc", "data.frame")
  res
}



.performance_roc_model <- function(x, new_data, model_name = "Model 1") {
  predictions <- stats::predict(x, newdata = new_data, type = "response")
  if (is.null(new_data)) new_data <- insight::get_data(x)
  response <- new_data[[insight::find_response(x)]]

  dat <- .performance_roc_numeric(response, predictions)
  dat$Model <- model_name
  dat
}



.performance_roc_models <- function(x, names) {
  l <- lapply(1:length(x), function(i) {
    if (inherits(x[[i]], "glm")) {
      .performance_roc_model(x = x[[i]], new_data = NULL, model_name = names[i])
    } else {
      warning("Object '", names[i], "' is not valid.", call. = FALSE)
    }
  })
  do.call(rbind, l)
}
