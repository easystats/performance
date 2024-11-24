#' @title Simple ROC curve
#' @name performance_roc
#'
#' @description
#' This function calculates a simple ROC curves of x/y coordinates based on
#' response and predictions of a binomial model.
#'
#' It returns the area under the curve (AUC) as a percentage, which corresponds
#' to the probability that a randomly chosen observation of "condition 1" is
#' correctly classified by the model as having a higher probability of being
#' "condition 1" than a randomly chosen "condition 2" observation.
#'
#' Applying `as.data.frame()` to the output returns a data frame containing the
#' following:
#' - `Sensitivity` (that actually corresponds to `1 - Specificity`): It is the
#'   False Positive Rate.
#' - `Sensitivity`: It is the True Positive Rate, which is the proportion of
#'   correctly classified "condition 1" observations.
#'
#' @param x A numeric vector, representing the outcome (0/1), or a model with
#'   binomial outcome.
#' @param predictions If `x` is numeric, a numeric vector of same length
#'   as `x`, representing the actual predicted values.
#' @param new_data If `x` is a model, a data frame that is passed to
#'   `predict()` as `newdata`-argument. If `NULL`, the ROC for
#'   the full model is calculated.
#' @param ... One or more models with binomial outcome. In this case,
#'   `new_data` is ignored.
#'
#' @note There is also a [`plot()`-method](https://easystats.github.io/see/articles/performance.html)
#' implemented in the \href{https://easystats.github.io/see/}{\pkg{see}-package}.
#'
#' @return A data frame with three columns, the x/y-coordinate pairs for the ROC
#'   curve (`Sensitivity` and `Specificity`), and a column with the
#'   model name.
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
#' as.data.frame(performance_roc(model, new_data = test_data))
#'
#' roc <- performance_roc(model, new_data = test_data)
#' area_under_curve(roc$Specificity, roc$Sensitivity)
#'
#' if (interactive()) {
#'   m1 <- glm(y ~ Sepal.Length + Sepal.Width, data = iris, family = "binomial")
#'   m2 <- glm(y ~ Sepal.Length + Petal.Width, data = iris, family = "binomial")
#'   m3 <- glm(y ~ Sepal.Length + Species, data = iris, family = "binomial")
#'   performance_roc(m1, m2, m3)
#'
#'   # if you have `see` package installed, you can also plot comparison of
#'   # ROC curves for different models
#'   if (require("see")) plot(performance_roc(m1, m2, m3))
#' }
#' @export
performance_roc <- function(x, ..., predictions, new_data) {
  dots <- list(...)

  object_names <- c(
    insight::safe_deparse_symbol(substitute(x)),
    sapply(match.call(expand.dots = FALSE)$`...`, insight::safe_deparse)
  )

  if (insight::is_model(x)) {
    info <- insight::model_info(x)
  } else {
    info <- NULL
  }

  if (is.numeric(x) && !missing(predictions) && !is.null(predictions)) {
    .performance_roc_numeric(x, predictions)
  } else if (inherits(x, c("logitor", "logitmfx", "probitmfx", "model_fit")) && length(dots) == 0) {
    if (missing(new_data)) new_data <- NULL
    .performance_roc_model(x$fit, new_data)
  } else if (info$is_binomial && length(dots) == 0) {
    if (missing(new_data)) new_data <- NULL
    .performance_roc_model(x, new_data)
  } else if (length(dots) > 0) {
    .performance_roc_models(list(x, ...), names = object_names)
  }
}



# methods -----------------------------

#' @export
plot.performance_roc <- function(x, ...) {
  insight::check_if_installed("see", "to plot ROC-curves")
  NextMethod()
}


#' @export
print.performance_roc <- function(x, ...) {
  if (length(unique(x$Model)) == 1) {
    cat(sprintf("AUC: %.2f%%\n", 100 * bayestestR::area_under_curve(x$Specificity, x$Sensitivity)))
  } else {
    insight::print_color("# Area under Curve\n\n", "blue")

    dat <- split(x, f = x$Model)
    max_space <- max(nchar(x$Model))

    for (i in seq_along(dat)) {
      cat(sprintf(
        "  %*s: %.2f%%\n",
        max_space,
        names(dat)[i],
        100 * bayestestR::area_under_curve(dat[[i]]$Specificity, dat[[i]]$Sensitivity)
      ))
    }
  }
  invisible(x)
}



# utilities ---------------------------

.performance_roc_numeric <- function(x, predictions) {
  if (length(x) != length(predictions)) {
    insight::format_error("`x` and `predictions` must be of same length.")
  }

  x <- .recode_to_zero(x)
  x <- x[order(predictions, decreasing = TRUE)]

  res <- data.frame(
    Sensitivity = c(0, cumsum(x) / sum(x), 1),
    Specificity = c(0, cumsum(!x) / sum(!x), 1)
  )

  class(res) <- c("performance_roc", "see_performance_roc", "data.frame")
  res
}



.performance_roc_model <- function(x, new_data, model_name = "Model 1") {
  predictions <- stats::predict(x, newdata = new_data, type = "response")
  if (is.null(new_data)) new_data <- insight::get_data(x, verbose = FALSE)
  response <- new_data[[insight::find_response(x)]]

  if ((is.data.frame(response) || is.matrix(response)) && ncol(response) > 1) {
    insight::format_error(
      "Can't calculate ROC for models with response-matrix (i.e. response variables with success/trials)."
    )
  }

  dat <- .performance_roc_numeric(response, predictions)
  dat$Model <- model_name
  dat
}



.performance_roc_models <- function(x, names) {
  l <- lapply(seq_along(x), function(i) {
    if (.valid_roc_models(x[[i]])) {
      .performance_roc_model(x = x[[i]], new_data = NULL, model_name = names[i])
    } else {
      insight::format_warning("Object '", names[i], "' is not valid.")
    }
  })
  do.call(rbind, l)
}



# add supported glm models here

.valid_roc_models <- function(x) {
  if (inherits(x, "model_fit")) {
    x <- x$fit
  }
  inherits(x, c("glm", "glmerMod", "logitor", "logitmfx", "probitmfx"))
}
