#' @title Accuracy of predictions from model fit
#' @name performance_accuracy
#'
#' @description This function calculates the predictive accuracy of linear
#'    or logistic regression models.
#'
#' @param model Fitted model object of class \code{lm} or \code{glm}, the latter
#'    being a logistic regression model (binary response).
#' @param k The number of folds for the kfold-crossvalidation.
#' @param method Character string, indicating whether crossvalidation
#'    (\code{method = "cv"}) or bootstrapping (\code{method = "boot"})
#'    is used to compute the accuracy values.
#' @param n Number of bootstrap-samples.
#'
#' @return A list with three values: The \code{Accuracy} of the model predictions, i.e.
#'    the proportion of accurately predicted values from the model, its standard
#'    error, \code{SE}, and the \code{Method} used to compute the accuracy.
#'
#' @details For linar models, the accuracy is the correlation coefficient
#'    between the actual and the predicted value of the outcome. For
#'    logistic regression models, the accuracy corresponds to the
#'    AUC-value, calculated with the \code{\link[bayestestR]{auc}}-function.
#'    \cr \cr
#'    The accuracy is the mean value of multiple correlation resp.
#'    AUC-values, which are either computed with crossvalidation
#'    or nonparametric bootstrapping (see argument \code{method}).
#'    The standard error is the standard deviation of the computed
#'    correlation resp. AUC-values.
#'
#' @examples
#' model <- lm(mpg ~ wt + cyl, data = mtcars)
#' performance_accuracy(model)
#'
#' model <- glm(vs ~ wt + mpg, data = mtcars, family = "binomial")
#' performance_accuracy(model)
#' @importFrom bayestestR area_under_curve
#' @importFrom insight find_response get_data
#' @importFrom stats lm cor glm predict predict.glm model.frame formula binomial sd
#' @export
performance_accuracy <- function(model, method = c("cv", "boot"), k = 5, n = 1000) {
  method <- match.arg(method)

  # get formula from model fit
  formula <- stats::formula(model)

  # get name of response
  resp.name <- insight::find_response(model)

  # model data, for cross validation
  model_data <- insight::get_data(model)

  # accuracy for linear models
  if (inherits(model, "lm") && !inherits(model, "glm")) {
    measure <- "Correlation between observed and predicted"

    # check if bootstrapping or cross validation is requested
    if (method == "boot") {

      # accuracy linear models with bootstrapping

      bootstr <- replicate(n, sample(nrow(model_data), replace = TRUE), simplify = FALSE)

      models <- lapply(bootstr, function(.x) {
        stats::lm(formula, data = model_data[.x, ])
      })

      predictions <- mapply(function(.x, .y) {
        stats::predict(.y, newdata = model_data[.x, ])
      }, bootstr, models, SIMPLIFY = FALSE)

      response <- lapply(bootstr, function(.x) {
        as.data.frame(model_data[.x, ])[[resp.name]]
      })

      accuracy <- mapply(function(.x, .y) {
        stats::cor(.x, .y, use = "pairwise.complete.obs")
      }, predictions, response)
    } else {

      # accuracy linear models with cross validation

      cv <- .crossv_kfold(model_data, k = k)

      models <- lapply(cv, function(.x) {
        stats::lm(formula, data = model_data[.x$train, ])
      })

      predictions <- mapply(function(.x, .y) {
        stats::predict(.y, newdata = model_data[.x$test, ])
      }, cv, models, SIMPLIFY = FALSE)

      response <- lapply(cv, function(.x) {
        as.data.frame(model_data[.x$test, ])[[resp.name]]
      })

      accuracy <- mapply(function(.x, .y) {
        stats::cor(.x, .y, use = "pairwise.complete.obs")
      }, predictions, response)
    }
  } else if (inherits(model, "glm") && stats::family(model)$family == "binomial") {
    measure <- "Area under Curve"

    # check if bootstrapping or cross validation is requested
    if (method == "boot") {

      # accuracy linear models with bootstrapping

      bootstr <- replicate(n, sample(nrow(model_data), replace = TRUE), simplify = FALSE)

      models <- lapply(bootstr, function(.x) {
        stats::glm(formula, data = model_data[.x, ], family = stats::binomial(link = "logit"))
      })

      predictions <- mapply(function(.x, .y) {
        stats::predict.glm(.y, newdata = model_data[.x, ])
      }, bootstr, models, SIMPLIFY = FALSE)

      response <- lapply(bootstr, function(.x) {
        .factor_to_numeric(as.data.frame(model_data[.x, ])[[resp.name]])
      })

      accuracy <- mapply(function(.x, .y) {
        roc <- performance_roc(x = .x, predictions = .y)
        bayestestR::area_under_curve(roc$Sensivity, roc$Specifity)
      }, response, predictions)
    } else {

      # accuracy linear models with cross validation
      cv <- .crossv_kfold(model_data, k = k)

      models <- lapply(cv, function(.x) {
        stats::glm(formula, data = model_data[.x$train, ], family = stats::binomial(link = "logit"))
      })

      predictions <- mapply(function(.x, .y) {
        stats::predict.glm(.y, newdata = model_data[.x$test, ])
      }, cv, models, SIMPLIFY = FALSE)

      response <- lapply(cv, function(.x) {
        .factor_to_numeric(as.data.frame(model_data[.x$test, ])[[resp.name]])
      })

      accuracy <- mapply(function(.x, .y) {
        roc <- performance_roc(x = .x, predictions = .y)
        bayestestR::area_under_curve(roc$Sensivity, roc$Specifity)
      }, response, predictions)
    }
  }

  # return mean value of accuracy
  structure(
    class = c("performance_accuracy"),
    list(
      Accuracy = mean(accuracy),
      SE = stats::sd(accuracy),
      Method = measure
    )
  )
}


.crossv_kfold <- function(model_data, k = 5) {
  n <- nrow(model_data)
  folds <- sample(rep(1:k, length.out = n))
  idx <- seq_len(n)
  fold_idx <- split(idx, folds)
  fold <- function(test) {
    list(train = setdiff(idx, test), test = test)
  }
  lapply(fold_idx, fold)
}
