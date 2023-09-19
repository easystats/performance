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
#' @param metrics Can be `"all"`, `"common"` or a character vector of metrics to be
#'   computed (some of `c("ELPD", "Deviance", "MSE", "RMSE", "R2")`). "common" will
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
performance_cv <- function(model,
                           data = NULL,
                           method = c("holdout", "k_fold", "loo"),
                           metrics = "all",
                           prop = 0.30,
                           k = 5,
                           stack = TRUE,
                           verbose = TRUE,
                           ...) {
  if (all(metrics == "all")) {
    metrics <- c("MSE", "RMSE", "R2")
  } else if (all(metrics == "common")) {
    metrics <- c("RMSE", "R2")
  } else {
    metrics <- toupper(metrics)
    metrics[metrics == "DEVIANCE"] <- "Deviance"
  }
  if (is.null(data)) {
    method <- match.arg(method, choices = c("holdout", "k_fold", "loo"))
  }
  if (!is.null(data) && inherits(model, "BFBayesFactor")) {
    insight::format_error("Models of class 'BFBayesFactor' not yet supported.")
  }
  resp.name <- insight::find_response(model)
  model_data <- insight::get_data(model, verbose = FALSE)
  info <- insight::model_info(model, verbose = verbose)
  if (info$is_linear) {
    if (!is.null(data)) {
      method <- "holdout"
      stack <- TRUE
      test_resp <- data[, resp.name]
      test_pred <- insight::get_predicted(model, ci = NULL, data = data)
      test_resd <- test_resp - test_pred
    } else if (method == "holdout") {
      train_i <- sample(seq_len(nrow(model_data)), size = round((1 - prop) * nrow(model_data)), replace = FALSE)
      model_upd <- stats::update(model, data = model_data[train_i, ])
      test_resp <- model_data[-train_i, resp.name]
      test_pred <- insight::get_predicted(model_upd, ci = NULL, data = model_data[-train_i, ])
      test_resd <- test_resp - test_pred
    } else if (method == "loo" && !info$is_bayesian) {
      model_response <- insight::get_response(model)
      MSE <- mean(insight::get_residuals(model, weighted = TRUE)^2 /
        (1 - stats::hatvalues(model))^2)
      mean(test_resd^2, na.rm = TRUE)
      RMSE <- sqrt(MSE)
      R2 <- 1 - MSE / (mean(model_response^2, na.rm = TRUE) - mean(model_response, na.rm = TRUE)^2)
      out <- data.frame(MSE = MSE, RMSE = RMSE, R2 = R2)
    } else {
      # Manual method for LOO, use this for non-linear and Bayesian models
      if (method == "loo") {
        if (info$is_bayesian && verbose) {
          insight::format_alert(
            "Simple LOO cross-validation can be very slow for MCMC models.",
            "Try loo::loo() instead."
          )
        }
        stack <- TRUE
        k <- nrow(model_data)
      }
      if (k > nrow(model_data)) {
        message(insight::color_text(insight::format_message(
          "Requested number of folds (k) larger than the sample size.",
          "'k' set equal to the sample size (leave-one-out [LOO])."
        ), color = "yellow"))
        k <- nrow(model_data)
      }
      cv_folds <- .crossv_kfold(model_data, k = k)
      models_upd <- lapply(cv_folds, function(.x) {
        stats::update(model, data = model_data[.x$train, ])
      })
      test_pred <- mapply(function(.x, .y) {
        insight::get_predicted(.y, ci = NULL, data = model_data[.x$test, ])
      }, cv_folds, models_upd, SIMPLIFY = FALSE)
      test_resp <- lapply(cv_folds, function(.x) {
        as.data.frame(model_data[.x$test, ])[[resp.name]]
      })
    }
  } else {
    insight::format_error("Only linear models currently supported.")
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
      MSE = mean(MSEs), MSE_SE = stats::sd(MSEs),
      RMSE = mean(RMSEs), RMSE_SE = stats::sd(RMSEs),
      R2 = mean(R2s), R2_SE = stats::sd(R2s)
    )
  }

  out <- out[, colnames(out) %in% c(metrics, paste0(metrics, "_SE"))]
  attr(out, "method") <- method
  attr(out, "k") <- if (method == "k_fold") k
  attr(out, "prop") <- if (method == "holdout") prop
  missing_metrics <- setdiff(metrics, c("MSE", "RMSE", "R2"))
  if (length(missing_metrics)) {
    message(insight::colour_text(insight::format_message(
      paste0(
        "Metric",
        ifelse(length(missing_metrics) > 1, "s '", " '"),
        paste0(missing_metrics, collapse = "', '"),
        "' not yet supported."
      )
    ), colour = "red"))
  }
  class(out) <- c("performance_cv", "data.frame")
  return(out)
}

# TODO: implement performance::log_lik() function for deviance/elpd metrics
#   - When given a model, it should pass it to insight::get_loglikelihood, stats4::logLik, stats::logLik, or rstantools::log_lik
#   - When given a model and new data, it should pass to rstantools::log_lik if stan
#     or compute a df like this:
#       df <- list(residuals = cv_residuals); class(df) <- class(model)
#     then pass this df to stats4::logLik or stats::logLik
#   - for model classes that do not compute their ll inside of logLik,
#       then compute the ll by running:
#       logLik(update(model, formula = {{response}} ~ 0, offset = predict(model, newdata), data = newdata))

# methods ----------------------------------

#' @export
print.performance_cv <- function(x, digits = 2, ...) {
  method <- switch(attr(x, "method"),
    holdout = paste0(
      insight::format_value(attr(x, "prop"), as_percent = TRUE, digits = 0),
      " holdout"
    ),
    k_fold = paste0(attr(x, "k"), "-fold"),
    loo = "leave-one-out [LOO]"
  )

  formatted_table <- format(
    x = x, digits = digits, format = "text",
    ...
  )
  cat(insight::export_table(
    x = formatted_table,
    digits = digits,
    format = "text",
    caption = c(paste0(
      "# Cross-validation performance (",
      method,
      " method)"
    ), "blue"),
    ...
  ))
  invisible(x)
}

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
