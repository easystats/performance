#' Classify the distribution of a model-family using machine learning
#'
#' @name classify_distribution
#' @docType data
#' @keywords data
#' @details
#' The trained model to classify distributions, which is used by the
#' `check_distribution()` function.
NULL


#' Classify the distribution of a model-family using machine learning
#'
#' Choosing the right distributional family for regression models is essential
#' to get more accurate estimates and standard errors. This function may help to
#' check a models' distributional family and see if the model-family probably
#' should be reconsidered. Since it is difficult to exactly predict the correct
#' model family, consider this function as somewhat experimental.
#'
#' @param model Typically, a model (that should response to `residuals()`).
#'   May also be a numeric vector.
#'
#' @note This function is somewhat experimental and might be improved in future
#'   releases. The final decision on the model-family should also be based on
#'   theoretical aspects and other information about the data and the model.
#'   \cr \cr
#'   There is also a
#'   [`plot()`-method](https://easystats.github.io/see/articles/performance.html)
#'   implemented in the
#'   \href{https://easystats.github.io/see/}{\pkg{see}-package}.
#'
#' @details
#'
#' This function uses an internal random forest model to classify the
#' distribution from a model-family. Currently, following distributions are
#' trained (i.e. results of `check_distribution()` may be one of the
#' following): `"bernoulli"`, `"beta"`, `"beta-binomial"`,
#' `"binomial"`, `"chi"`, `"exponential"`, `"F"`,
#' `"gamma"`, `"lognormal"`, `"normal"`, `"negative
#' binomial"`, `"negative binomial (zero-inflated)"`, `"pareto"`,
#' `"poisson"`, `"poisson (zero-inflated)"`, `"uniform"` and
#' `"weibull"`.
#' \cr \cr
#' Note the similarity between certain distributions according to shape, skewness,
#' etc. Thus, the predicted distribution may not be perfectly representing the
#' distributional family of the underlying fitted model, or the response value.
#' \cr \cr
#' There is a `plot()` method, which shows the probabilities of all predicted
#' distributions, however, only if the probability is greater than zero.
#'
#' @examples
#' if (require("lme4") && require("parameters") &&
#'   require("see") && require("patchwork") && require("randomForest")) {
#'   data(sleepstudy)
#'
#'   model <<- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
#'   check_distribution(model)
#'   plot(check_distribution(model))
#' }
#' @export
check_distribution <- function(model) {
  UseMethod("check_distribution")
}



# default -----------------------------

#' @export
check_distribution.default <- function(model) {
  insight::check_if_installed("randomForest")

  if (inherits(model, "brmsfit")) {
    x <- stats::residuals(model)[, "Estimate"]
  } else {
    x <- stats::residuals(model)
  }
  # x_scaled <- .normalize(x)
  dat <- .extract_features(x)

  dist_residuals <- as.data.frame(t(stats::predict(classify_distribution, dat, type = "prob")))


  # Extract features
  x <- datawizard::to_numeric(insight::get_response(model, verbose = FALSE), dummy_factors = FALSE, preserve_levels = TRUE)
  dat <- .extract_features(x)

  dist_response <- as.data.frame(t(stats::predict(classify_distribution, dat, type = "prob")))

  out <- data.frame(
    Distribution = rownames(dist_response),
    p_Residuals = dist_residuals[[1]],
    p_Response = dist_response[[1]],
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  class(out) <- unique(c("check_distribution", "see_check_distribution", class(out)))
  attr(out, "data") <- model
  attr(out, "object_name") <- deparse(substitute(model), width.cutoff = 500)

  out
}



# methods --------------------------

#' @export
print.check_distribution <- function(x, ...) {
  insight::print_color("# Distribution of Model Family\n\n", "blue")

  x1 <- x[order(x$p_Residuals, decreasing = TRUE)[1:3], c(1, 2)]
  x1 <- x1[x1$p_Residuals > 0, ]
  x1$p_Residuals <- sprintf("%g%%", round(100 * x1$p_Residuals))
  colnames(x1) <- c("Distribution", "Probability")

  insight::print_color("Predicted Distribution of Residuals\n\n", "red")
  print.data.frame(x1, row.names = FALSE, ...)

  x2 <- x[order(x$p_Response, decreasing = TRUE)[1:3], c(1, 3)]
  x2 <- x2[x2$p_Response > 0, ]
  x2$p_Response <- sprintf("%g%%", round(100 * x2$p_Response))
  colnames(x2) <- c("Distribution", "Probability")

  insight::print_color("\nPredicted Distribution of Response\n\n", "red")
  print.data.frame(x2, row.names = FALSE, ...)
  invisible(x)
}


#' @export
print.check_distribution_numeric <- function(x, ...) {
  insight::print_color("# Predicted Distribution of Vector\n\n", "blue")

  x1 <- x[order(x$p_Vector, decreasing = TRUE)[1:3], c(1, 2)]
  x1 <- x1[x1$p_Vector > 0, ]
  x1$p_Vector <- sprintf("%g%%", round(100 * x1$p_Vector))
  colnames(x1) <- c("Distribution", "Probability")

  print.data.frame(x1, row.names = FALSE, ...)
  invisible(x)
}


#' @export
plot.check_distribution <- function(x, ...) {
  insight::check_if_installed("see", "to plot predicted distributions")
  NextMethod()
}


#' @export
plot.check_distribution_numeric <- function(x, ...) {
  insight::check_if_installed("see", "to plot predicted distributions")
  NextMethod()
}



# other classes -------------------

#' @export
check_distribution.numeric <- function(model) {
  insight::check_if_installed("randomForest")

  dat <- .extract_features(model)
  dist <- as.data.frame(t(stats::predict(classify_distribution, dat, type = "prob")))

  out <- data.frame(
    Distribution = rownames(dist),
    p_Vector = dist[[1]],
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  class(out) <- unique(c("check_distribution_numeric", "see_check_distribution_numeric", class(out)))
  attr(out, "data") <- model

  out
}



# utilities -----------------------------

.extract_features <- function(x) {
  data.frame(
    "SD" = stats::sd(x),
    "MAD" = stats::mad(x, constant = 1),
    "Mean_Median_Distance" = mean(x) - stats::median(x),
    "Mean_Mode_Distance" = mean(x) - as.numeric(bayestestR::map_estimate(x, bw = "nrd0")),
    "SD_MAD_Distance" = stats::sd(x) - stats::mad(x, constant = 1),
    "Var_Mean_Distance" = stats::var(x) - mean(x),
    "Range_SD" = diff(range(x)) / stats::sd(x),
    "Range" = diff(range(x)),
    "IQR" = stats::IQR(x),
    "Skewness" = .skewness(x),
    "Kurtosis" = .kurtosis(x),
    "Uniques" = length(unique(x)) / length(x),
    "N_Uniques" = length(unique(x)),
    "Min" = min(x),
    "Max" = max(x),
    "Proportion_Positive" = sum(x >= 0) / length(x),
    "Integer" = all(.is_integer(x))
  )
}



.is_integer <- function(x) {
  tryCatch(
    expr = {
      ifelse(is.infinite(x), FALSE, x %% 1 == 0)
    },
    warning = function(w) {
      is.integer(x)
    },
    error = function(e) {
      FALSE
    }
  )
}
