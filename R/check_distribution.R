#' Classify the distribution of a model-family using machine learning
#'
#' Choosing the right distributional family for regression models is essential
#' to get more accurate estimates and standard errors. This function may help to
#' Machine learning model trained to classify distributions
#'
#' Mean accuracy and Kappa of 0.86 and 0.85, repsectively.
#'
"classify_distribution"


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
#' if (require("lme4") && require("parameters") && require("see") && require("patchwork")) {
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
  x <- .factor_to_numeric(insight::get_response(model, verbose = FALSE))
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
  attr(out, "object_name") <- deparse(substitute(model), width.cutoff = 500)

  out
}



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
