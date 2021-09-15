#' @title Cross-validated model performance
#' @name performance_cv
#'
#' @description This function cross-validates regression models in a
#'   user-supplied new sample or by using holdout (train-test), k-fold, or
#'   leave-one-out cross-validation.
#'
#' @param model A regression model.
#' @param data Optional. A data frame containing the same variables as `model`
#'   that will be used as the cross-validation sample.
#' @param method Character string, indicating the cross-validation method to use:
#'   whether holdout (`"holdout"`, aka train-test), k-fold (`"k_fold"`), or
#'   leave-one-out (`"loo"`). If `data` is supplied, this argument is ignored.
#' @param metrics Can be "all", "common" or a character vector of metrics to be
#'   computed (some of c("Deviance", "MSE", "RMSE", "R2")). "common" will
#'   compute R2 and RMSE.
#' @param prop If `method = "holdout"`, what proportion of the sample to hold
#'   out as the test sample?
#' @param k If `method = "k_fold"`, the number of folds to use.
#' @param stack Logical. If `method = "k_fold"`, should performance be computed
#'   by stacking residuals from each holdout fold and calculating each metric on
#'   the stacked data (`TRUE`, default) or should performance be computed by
#'   calculating metrics within each holdout fold and averaging performance
#'   across each fold (`FALSE`)?
#' @param verbose Toggle warnings.
#' @param ... Not used.
#'
#' @return A data frame with columns for each metric requested, as well as `k`
#'   if `method = "holdout"` and the `Method` used for cross-validation. If
#'   `method = "holdout"` and `stack = TRUE`, the standard error (standard
#'   deviation across holdout folds) for each metric is also included.
#'
#' @examples
#' model <- lm(mpg ~ wt + cyl, data = mtcars)
#' performance_cv(model)
#'
#' @export
performance_cv <- function(
  model,
  data = NULL,
  method = c("holdout", "k_fold", "loo"),
  metrics = "all",
  prop = .30,
  k = 5,
  stack = TRUE,
  verbose = TRUE,
  ...
) {
  if (metrics == "all") {
    metrics <- c("MSE", "RMSE", "R2")
  } else if (metrics == "common") {
    metrics <- c("RMSE", "R2")
  }
  method <- match.arg(method, choices = c("holdout", "k_fold"))
  formula <- stats::formula(model)
  resp.name <- insight::find_response(model)
  model_data <- insight::get_data(model, verbose = verbose)
  info <- insight::model_info(model, verbose = verbose)
  if (info$is_linear) {
    if (!is.null(data)) {
      stack <- TRUE
      test_resp <- data[, resp.name]
      test_pred <- insight::get_predicted(model, data = data)
      test_resd <- test_resp - test_pred
    } else if (method == "holdout") {
      train_i <- sample(seq_len(nrow(model_data)), size = round((1 - prop) * nrow(model_data)), replace = FALSE)
      model_upd <- stats::update(model, data = model_data[train_i,])
      test_resp <- model_data[-train_i, resp.name]
      test_pred <- insight::get_predicted(model_upd, data = model_data[-train_i, ])
      test_resd <- test_resp - test_pred
    } else {
      if (method == "loo") {
        stack <- TRUE
        k = length(test_pred)
      }
      cv_folds <- .crossv_kfold(model_data, k = k)
      models_upd <- lapply(cv_folds, function(.x) {
        stats::update(model, data = model_data[.x$train, ])
      })
      test_pred <- mapply(function(.x, .y) {
        insight::get_predicted(.y, data = model_data[.x$test, ])
      }, cv_folds, models_upd, SIMPLIFY = FALSE)
      test_resp <- lapply(cv_folds, function(.x) {
        as.data.frame(model_data[.x$test, ])[[resp.name]]
      })
    }
  } else {
    warning("Only linear models currently supported.", call. = FALSE)
    return(NULL)
  }
  if (isTRUE(stack)) {
    test_resp <- unlist(test_resp)
    test_pred <- unlist(test_pred)
    test_resd <- test_resp - test_pred
    MSE <- mean(test_resd^2, na.rm = TRUE)
    RMSE <- sqrt(MSE)
    R2 <- 1 - MSE / mean((test_resp - mean(test_resp, na.rm = TRUE))^2, na.rm = TRUE)
    out <- data.frame(MSE = MSE, RMSE = RMSE, R2 = R2)
  } else {
    test_resd <- mapply(function(.x, .y) {
      .x - .y
    }, test_resp, test_pred, SIMPLIFY = FALSE)
    MSEs <- sapply(test_resd, function(x) mean(x^2, na.rm = TRUE))
    RMSEs <- sqrt(MSEs)
    resp_vars <- sapply(test_resp, function(x) mean((x - mean(x, na.rm = TRUE))^2, na.rm = TRUE))
    R2s <- 1 - MSEs / resp_vars
    out <- data.frame(
      MSE = mean(MSEs), MSE_SE = sd(MSEs),
      RMSE = mean(RMSEs), RMSE_SE = sd(RMSEs),
      R2 = mean(R2s), R2_SE = sd(R2s)
    )
  }

  out <- out[, colnames(out) %in% c(metrics, paste0(metrics, "_SE"))]
  out$Method <- method
  out$k <- if (method == "k_fold") k else NULL
  if ("deviance" %in% metrics) mesage("Metric 'deviance' not yet implemented.")
  class(out) <- c("performance_cv", "data.frame")
  return(out)
}

# TODO: implement performance::log_lik() function
#   - When given a model, it should pass it to stats4::logLik, stats::logLik, or rstantools::log_lik
#   - When given a model and new data, it should pass to rstantools::log_lik if stan
#     or compute a df like this:
#       df <- list(residuals = cv_residuals); class(df) <- class(model)
#     then pass this df to stats4::logLik or stats::logLik
#   - for model classes that do not compute their ll inside of logLik,
#       then compute the ll by running:
#       logLik(update(model, formula = {{response}} ~ 0, offset = predict(model, newdata), data = newdata))

#' @export
as.data.frame.performance_cv <- function(x, row.names = NULL, ...) {
  data.frame(
    Accuracy = x$Accuracy,
    SE = x$SE,
    Method = x$Method,
    stringsAsFactors = FALSE,
    row.names = row.names,
    ...
  )
}
