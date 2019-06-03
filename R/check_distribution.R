#' Machine learning model trained to classify distributions
#'
#' Mean accuracy and Kappa of 0.86 and 0.85, repsectively.
#'
"classify_distribution"





#' Classify the distribution of a model-family using machine learning
#'
#' This function uses an internal random forest model to classify the distribution from a model-family.
#'
#' @param model A model (that should response to \code{residuals()}).
#'
#' @note This function is somewhat experimental and might be improved in future releases.
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

  # Extract features
  dat <- data.frame(
    "SD" = stats::sd(x),
    "MAD" = stats::mad(x, constant = 1),
    "Mean_Median_Distance" = mean(x) - median(x),
    "Mean_Mode_Distance" = mean(x) - as.numeric(bayestestR::map_estimate(x, bw = "nrd0")),
    "SD_MAD_Distance" = stats::sd(x) - stats::mad(x, constant = 1),
    "Range" = diff(range(x)) / stats::sd(x),
    "IQR" = stats::IQR(x),
    "Skewness" = .skewness(x),
    "Kurtosis" = .kurtosis(x),
    "Uniques" = length(unique(x)) / length(x)
  )

  dist_residuals <- as.data.frame(t(stats::predict(classify_distribution, dat, type = "prob")))


  x <- insight::get_response(model)

  # Extract features
  dat <- data.frame(
    "SD" = stats::sd(x),
    "MAD" = stats::mad(x, constant = 1),
    "Mean_Median_Distance" = mean(x) - median(x),
    "Mean_Mode_Distance" = mean(x) - as.numeric(bayestestR::map_estimate(x, bw = "nrd0")),
    "SD_MAD_Distance" = stats::sd(x) - stats::mad(x, constant = 1),
    "Range" = diff(range(x)) / stats::sd(x),
    "IQR" = stats::IQR(x),
    "Skewness" = .skewness(x),
    "Kurtosis" = .kurtosis(x),
    "Uniques" = length(unique(x)) / length(x)
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
