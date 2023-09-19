#' @title Accuracy of predictions from model fit
#' @name performance_accuracy
#'
#' @description This function calculates the predictive accuracy of linear
#'    or logistic regression models.
#'
#' @param model A linear or logistic regression model. A mixed-effects model is
#'   also accepted.
#' @param k The number of folds for the k-fold cross-validation.
#' @param method Character string, indicating whether cross-validation
#'   (`method = "cv"`) or bootstrapping (`method = "boot"`) is used to
#'   compute the accuracy values.
#' @param n Number of bootstrap-samples.
#' @param verbose Toggle warnings.
#' @inheritParams performance_pcp
#'
#' @return A list with three values: The `Accuracy` of the model
#'   predictions, i.e. the proportion of accurately predicted values from the
#'   model, its standard error, `SE`, and the `Method` used to compute
#'   the accuracy.
#'
#' @details For linear models, the accuracy is the correlation coefficient
#'    between the actual and the predicted value of the outcome. For
#'    logistic regression models, the accuracy corresponds to the
#'    AUC-value, calculated with the `bayestestR::auc()`-function.
#'    \cr \cr
#'    The accuracy is the mean value of multiple correlation resp.
#'    AUC-values, which are either computed with cross-validation
#'    or non-parametric bootstrapping (see argument `method`).
#'    The standard error is the standard deviation of the computed
#'    correlation resp. AUC-values.
#'
#' @examples
#' model <- lm(mpg ~ wt + cyl, data = mtcars)
#' performance_accuracy(model)
#'
#' model <- glm(vs ~ wt + mpg, data = mtcars, family = "binomial")
#' performance_accuracy(model)
#' @export
performance_accuracy <- function(model,
                                 method = c("cv", "boot"),
                                 k = 5,
                                 n = 1000,
                                 ci = 0.95,
                                 verbose = TRUE) {
  method <- match.arg(method)

  # get formula from model fit
  formula <- stats::formula(model)

  # get name of response
  resp.name <- insight::find_response(model)

  # model data, for cross validation
  model_data <- insight::get_data(model, verbose = FALSE)

  info <- insight::model_info(model, verbose = verbose)

  # accuracy for linear models
  if (info$is_linear) {
    measure <- "Correlation between observed and predicted"

    # check if bootstrapping or cross validation is requested
    if (method == "boot") {
      # accuracy linear models with bootstrapping

      bootstr <- replicate(n, sample(nrow(model_data), replace = TRUE), simplify = FALSE)

      models <- lapply(bootstr, function(.x) {
        text <- utils::capture.output({
          model_upd <- stats::update(model, data = model_data[.x, ])
        })
        # stats::lm(formula, data = model_data[.x, ])
        model_upd
      })

      predictions <- Map(function(.x, .y) {
        stats::predict(.y, newdata = model_data[.x, ])
      }, bootstr, models)

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
        text <- utils::capture.output({
          model_upd <- stats::update(model, data = model_data[.x$train, ])
        })
        model_upd
        # stats::lm(formula, data = model_data[.x$train, ])
      })

      predictions <- Map(function(.x, .y) {
        stats::predict(.y, newdata = model_data[.x$test, ])
      }, cv, models)

      response <- lapply(cv, function(.x) {
        as.data.frame(model_data[.x$test, ])[[resp.name]]
      })

      accuracy <- mapply(function(.x, .y) {
        stats::cor(.x, .y, use = "pairwise.complete.obs")
      }, predictions, response)
    }
  } else if (info$is_binomial) {
    measure <- "Area under Curve"

    # check if bootstrapping or cross validation is requested
    if (method == "boot") {
      # accuracy linear models with bootstrapping

      bootstr <- replicate(n, sample(nrow(model_data), replace = TRUE), simplify = FALSE)

      models <- lapply(bootstr, function(.x) {
        text <- utils::capture.output({
          model_upd <- stats::update(model, data = model_data[.x, ])
        })
        # stats::glm(formula, data = model_data[.x, ], family = stats::binomial(link = "logit"))
        model_upd
      })

      predictions <- Map(function(.x, .y) {
        stats::predict(.y, newdata = model_data[.x, ], type = "link")
      }, bootstr, models)

      response <- lapply(bootstr, function(.x) {
        .recode_to_zero(as.data.frame(model_data[.x, ])[[resp.name]])
      })

      accuracy <- mapply(function(.x, .y) {
        roc <- performance_roc(x = .x, predictions = .y)
        bayestestR::area_under_curve(roc$Specificity, roc$Sensitivity)
      }, response, predictions)
    } else {
      # accuracy linear models with cross validation
      cv <- .crossv_kfold(model_data, k = k)

      models <- lapply(cv, function(.x) {
        text <- utils::capture.output({
          model_upd <- stats::update(model, data = model_data[.x$train, ])
        })
        model_upd
        # stats::glm(formula, data = model_data[.x$train, ], family = stats::binomial(link = "logit"))
      })

      predictions <- Map(function(.x, .y) {
        stats::predict(.y, newdata = model_data[.x$test, ], type = "link")
      }, cv, models)

      response <- lapply(cv, function(.x) {
        .recode_to_zero(as.data.frame(model_data[.x$test, ])[[resp.name]])
      })

      accuracy <- mapply(function(.x, .y) {
        roc <- performance_roc(x = .x, predictions = .y)
        bayestestR::area_under_curve(roc$Specificity, roc$Sensitivity)
      }, response, predictions)
    }

    if (anyNA(accuracy)) {
      m <- ifelse(method == "cv", "cross-validated", "bootstrapped")
      if (verbose) {
        insight::format_alert(
          paste0("Some of the ", m, " samples were not eligible for calculating AUC.")
        )
      }
    }
  } else {
    if (verbose) {
      insight::format_warning(
        paste0("Models of class '", class(model)[1], "' are not supported.")
      )
    }
    return(NULL)
  }

  # return mean value of accuracy
  structure(
    class = "performance_accuracy",
    list(
      Accuracy = mean(accuracy, na.rm = TRUE),
      SE = stats::sd(accuracy, na.rm = TRUE),
      CI = ci,
      CI_low = as.vector(stats::quantile(accuracy, 1 - ((1 + ci) / 2), na.rm = TRUE)),
      CI_high = as.vector(stats::quantile(accuracy, (1 + ci) / 2, na.rm = TRUE)),
      Method = measure
    )
  )
}


# methods --------------------------

#' @export
as.data.frame.performance_accuracy <- function(x, row.names = NULL, ...) {
  data.frame(
    Accuracy = x$Accuracy,
    SE = x$SE,
    CI = x$CI,
    CI_low = x$CI_low,
    CI_high = x$CI_high,
    Method = x$Method,
    stringsAsFactors = FALSE,
    row.names = row.names,
    ...
  )
}


#' @export
print.performance_accuracy <- function(x, ...) {
  # headline
  insight::print_color("# Accuracy of Model Predictions\n\n", "blue")

  # statistics
  cat(sprintf(
    "Accuracy (%i%% CI): %.2f%% [%.2f%%, %.2f%%]\nMethod: %s\n",
    round(100 * x$CI),
    100 * x$Accuracy,
    100 * x$CI_low,
    100 * x$CI_high,
    x$Method
  ))

  invisible(x)
}



# utilities ------------------------

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
