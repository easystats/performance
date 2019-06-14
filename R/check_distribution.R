#' Machine learning model trained to classify distributions
#'
#' Mean accuracy and Kappa of 0.86 and 0.85, repsectively.
#'
"classify_distribution"





#' Classify the distribution of a model-family using machine learning
#'
#' Choosing the right distributional family for regression models is essential to get more accurate estimates and standard errors. This function may help to check a models' distributional family and see if the model-family probably should be reconsidered. Since it is difficult to exactly predict the correct model family, consider this function as somewhat experimental.
#'
#' @param model A model (that should response to \code{residuals()}).
#'
#' @note This function is somewhat experimental and might be improved in future releases.
#'   The final decision on the model-family should also be based on theoretical
#'   aspects and other information about the data and the model.
#'
#' @details This function uses an internal random forest model to classify the
#' distribution from a model-family. Currently, following distributions are
#' trained (i.e. results of \code{check_distribution()} may be one of the following):
#' \code{"bernoulli"}, \code{"beta"}, \code{"beta-binomial"}, \code{"binomial"},
#' \code{"chi"}, \code{"exponential"}, \code{"F"}, \code{"gamma"}, \code{"lognormal"},
#' \code{"normal"}, \code{"negative binomial"}, \code{"negative binomial (zero-inflated)"},
#' \code{"pareto"}, \code{"poisson"}, \code{"poisson (zero-inflated)"},
#' \code{"uniform"} and \code{"weibull"}.
#' \cr \cr
#' Note the similarity between certain distributions according to shape, skewness,
#' etc., for instance \code{plot(dnorm(1:100, 30, 3))} and \code{plot(dnorm(1:100, 30, 3))}.
#' Thus, the predicted distribution may not be perfectly matching to the underlying
#' fitted model.
#'
#' @examples
#' library(lme4)
#' model <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
#' check_distribution(model)
#'
#' @importFrom bayestestR map_estimate
#' @importFrom stats IQR density predict sd mad residuals
#' @importFrom insight get_response
#' @export
check_distribution <- function(model) {

  if (!requireNamespace("randomForest", quietly = TRUE)) {
    stop("Package `randomForest` required for this function to work. Please install it.", call. = FALSE)
  }

  x <- stats::residuals(model)
  # x_scaled <- .normalize(x)

  # Extract features
  dat <- data.frame(
    "SD" = stats::sd(x),
    "MAD" = stats::mad(x, constant = 1),
    "Mean_Median_Distance" = mean(x) - median(x),
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
    "Max" = max(x)
  )

  dist_residuals <- as.data.frame(t(stats::predict(classify_distribution, dat, type = "prob")))


  x <- .factor_to_numeric(insight::get_response(model))

  # Extract features
  dat <- data.frame(
    "SD" = stats::sd(x),
    "MAD" = stats::mad(x, constant = 1),
    "Mean_Median_Distance" = mean(x) - median(x),
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
    "Max" = max(x)
  )

  dist_response <- as.data.frame(t(stats::predict(classify_distribution, dat, type = "prob")))

  out <- data.frame(
    Distribution = rownames(dist_response),
    p_Residuals = dist_residuals[[1]],
    p_Response = dist_response[[1]],
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  class(out) <- c("check_distribution", class(out))
  out
}
