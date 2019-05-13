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
#' \code{predict()} as \code{newdata}-argument.
#'
#' @return A data frame with two columns, the x/y-coordinate pairs for the ROC
#' curve (\code{Sensivity} and \code{Specifity}), which, for instance, can be
#' used to further compute the AUC.
#'
#' @examples
#' library(bayestestR)
#' data(iris)
#' set.seed(123)
#' iris$y <- rbinom(nrow(iris), size = 1, .3)
#' folds <- sample(nrow(iris), size = nrow(iris) / 8, replace = FALSE)
#' test_data <- iris[folds, ]
#' train_data <- iris[-folds, ]
#'
#' model <- glm(y ~ Sepal.Length + Sepal.Width, data = train_data, family = "binomial")
#' performance_roc(model, test_data)
#'
#' roc <- performance_roc(model, test_data)
#' area_under_curve(roc$Sensivity, roc$Specifity)
#'
#' @export
performance_roc <- function(x, ...) {
  UseMethod("performance_roc")
}


#' @rdname performance_roc
#' @export
performance_roc.numeric <- function(x, predictions, ...){
  x <- x[order(predictions, decreasing = TRUE)]

  res <- data.frame(
    Sensivity = cumsum(x) / sum(x),
    Specifity = cumsum(!x) / sum(!x)
  )

  class(res) <- c("performance_roc", "see_performance_roc", "data.frame")
  res
}


#' @importFrom stats predict
#' @importFrom insight find_response get_data
#' @rdname performance_roc
#' @export
performance_roc.glm <- function(x, new_data = NULL, ...){
  predictions <- stats::predict(x, newdata = new_data, type = "response")
  if (is.null(new_data)) new_data <- insight::get_data(x)
  response <- new_data[[insight::find_response(x)]]
  performance_roc(response, predictions)
}
