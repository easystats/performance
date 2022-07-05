#' @title Compare performance of different models
#' @name compare_performance
#'
#' @description `compare_performance()` computes indices of model
#'   performance for different models at once and hence allows comparison of
#'   indices across models.
#'
#' @param ... Multiple model objects (also of different classes).
#' @param metrics Can be `"all"`, `"common"` or a character vector of
#'   metrics to be computed. See related
#'   [`documentation()`][model_performance] of object's class for
#'   details.
#' @param rank Logical, if `TRUE`, models are ranked according to 'best'
#'   overall model performance. See 'Details'.
#' @inheritParams performance_aic
#'
#' @return A data frame with one row per model and one column per "index" (see
#'   `metrics`).
#'
#' @note There is also a [`plot()`-method](https://easystats.github.io/see/articles/performance.html) implemented in the \href{https://easystats.github.io/see/}{\pkg{see}-package}.
#'
#' @details \subsection{Model Weights}{
#'   When information criteria (IC) are requested in `metrics` (i.e., any of `"all"`,
#'   `"common"`, `"AIC"`, `"AICc"`, `"BIC"`, `"WAIC"`, or `"LOOIC"`), model
#'   weights based on these criteria are also computed. For all IC except LOOIC,
#'   weights are computed as `w = exp(-0.5 * delta_ic) / sum(exp(-0.5 * delta_ic))`,
#'   where `delta_ic` is the difference between the model's IC value and the
#'   smallest IC value in the model set (Burnham and Anderson, 2002).
#'   For LOOIC, weights are computed as "stacking weights" using
#'   [loo::stacking_weights()].
#' }
#'
#' \subsection{Ranking Models}{
#'   When `rank = TRUE`, a new column `Performance_Score` is returned.
#'   This score ranges from 0\% to 100\%, higher values indicating better model
#'   performance. Note that all score value do not necessarily sum up to 100\%.
#'   Rather, calculation is based on normalizing all indices (i.e. rescaling
#'   them to a range from 0 to 1), and taking the mean value of all indices for
#'   each model. This is a rather quick heuristic, but might be helpful as
#'   exploratory index.
#'   \cr \cr
#'   In particular when models are of different types (e.g. mixed models,
#'   classical linear models, logistic regression, ...), not all indices will be
#'   computed for each model. In case where an index can't be calculated for a
#'   specific model type, this model gets an `NA` value. All indices that
#'   have any `NA`s are excluded from calculating the performance score.
#'   \cr \cr
#'   There is a `plot()`-method for `compare_performance()`,
#'   which creates a "spiderweb" plot, where the different indices are
#'   normalized and larger values indicate better model performance.
#'   Hence, points closer to the center indicate worse fit indices
#'   (see [online-documentation](https://easystats.github.io/see/articles/performance.html)
#'   for more details).
#' }
#'
#' \subsection{REML versus ML estimator}{
#' By default, `estimator = "ML"`, which means that values from information
#' criteria (AIC, AICc, BIC) for specific model classes (like models from *lme4*)
#' are based on the ML-estimator, while the default behaviour of `AIC()` for
#' such classes is setting `REML = TRUE`. This default is intentional, because
#' comparing information criteria based on REML fits is usually not valid
#' (it might be useful, though, if all models share the same fixed effects -
#' however, this is usually not the case for nested models, which is a
#' prerequisite for the LRT). Set `estimator = "REML"` explicitly return the
#' same (AIC/...) values as from the defaults in `AIC.merMod()`.
#' }
#'
#' @references
#' Burnham, K. P., and Anderson, D. R. (2002).
#' _Model selection and multimodel inference: A practical information-theoretic approach_ (2nd ed.).
#' Springer-Verlag. \doi{10.1007/b97636}
#'
#' @examples
#' data(iris)
#' lm1 <- lm(Sepal.Length ~ Species, data = iris)
#' lm2 <- lm(Sepal.Length ~ Species + Petal.Length, data = iris)
#' lm3 <- lm(Sepal.Length ~ Species * Petal.Length, data = iris)
#' compare_performance(lm1, lm2, lm3)
#' compare_performance(lm1, lm2, lm3, rank = TRUE)
#'
#' if (require("lme4")) {
#'   m1 <- lm(mpg ~ wt + cyl, data = mtcars)
#'   m2 <- glm(vs ~ wt + mpg, data = mtcars, family = "binomial")
#'   m3 <- lmer(Petal.Length ~ Sepal.Length + (1 | Species), data = iris)
#'   compare_performance(m1, m2, m3)
#' }
#' @inheritParams model_performance.lm
#' @export
compare_performance <- function(..., metrics = "all", rank = FALSE, estimator = "ML", verbose = TRUE) {
  # process input
  objects <- insight::ellipsis_info(..., only_models = TRUE)

  # ensure proper object names
  objects <- .check_objectnames(objects, sapply(match.call(expand.dots = FALSE)$`...`, as.character))

  # drop unsupport models
  supported_models <- sapply(objects, function(i) insight::is_model_supported(i) | inherits(i, "lavaan"))
  object_names <- names(objects)

  if (!all(supported_models)) {
    warning(sprintf("Following objects are not supported: %s", paste0(object_names[!supported_models], collapse = ", ")))
    objects <- objects[supported_models]
    object_names <- object_names[supported_models]
  }

  # iterate over all models, i.e. model-performance for each model
  m <- mapply(function(.x, .y) {
    dat <- model_performance(.x, metrics = metrics, estimator = estimator, verbose = FALSE)
    model_name <- gsub("\"", "", insight::safe_deparse(.y), fixed = TRUE)
    perf_df <- data.frame(Name = model_name, Model = class(.x)[1], dat, stringsAsFactors = FALSE)
    attributes(perf_df) <- c(attributes(perf_df), attributes(dat)[!names(attributes(dat)) %in% c("names", "row.names", "class")])
    return(perf_df)
  }, objects, object_names, SIMPLIFY = FALSE)

  attri <- lapply(m, function(x) {
    attri <- attributes(x)
    attri[!names(attri) %in% c("names", "row.names", "class")]
  })
  dfs <- Reduce(function(x, y) merge(x, y, all = TRUE, sort = FALSE), m)

  if (any(c("AIC", "AICc", "BIC", "WAIC") %in% names(dfs))) {
    dfs$AIC_wt <- .ic_weight(dfs[["AIC"]])
    dfs$AICc_wt <- .ic_weight(dfs[["AICc"]])
    dfs$BIC_wt <- .ic_weight(dfs[["BIC"]])
    dfs$WAIC_wt <- .ic_weight(dfs[["WAIC"]])
  }

  if ("LOOIC" %in% names(dfs)) {
    lpd_point <- do.call(cbind, lapply(attri, function(x) x$loo$pointwise[, "elpd_loo"]))
    dfs$LOOIC_wt <- as.numeric(loo::stacking_weights(lpd_point))
  }

  # check if all models were fit from same data
  if (!isTRUE(attributes(objects)$same_response) && verbose) {
    warning(insight::format_message("When comparing models, please note that probably not all models were fit from same data."), call. = FALSE)
  }

  # create "ranking" of models
  if (isTRUE(rank)) {
    dfs <- .rank_performance_indices(dfs, verbose)
  }

  # Reorder columns
  if (all(c("BIC", "BF") %in% names(dfs))) {
    idx1 <- grep("^BIC$", names(dfs))
    idx2 <- grep("BF", names(dfs))
    last_part <- (idx1 + 1):ncol(dfs)
    dfs <- dfs[, c(1:idx1, idx2, last_part[last_part != idx2])]
  }
  if (all(c("AIC", "AIC_wt") %in% names(dfs))) {
    idx1 <- grep("^AIC$", names(dfs))
    idx2 <- grep("AIC_wt", names(dfs))
    last_part <- (idx1 + 1):ncol(dfs)
    dfs <- dfs[, c(1:idx1, idx2, last_part[last_part != idx2])]
  }
  if (all(c("BIC", "BIC_wt") %in% names(dfs))) {
    idx1 <- grep("^BIC$", names(dfs))
    idx2 <- grep("BIC_wt", names(dfs))
    last_part <- (idx1 + 1):ncol(dfs)
    dfs <- dfs[, c(1:idx1, idx2, last_part[last_part != idx2])]
  }
  if (all(c("AICc", "AICc_wt") %in% names(dfs))) {
    idx1 <- grep("^AICc$", names(dfs))
    idx2 <- grep("AICc_wt", names(dfs))
    last_part <- (idx1 + 1):ncol(dfs)
    dfs <- dfs[, c(1:idx1, idx2, last_part[last_part != idx2])]
  }
  if (all(c("WAIC", "WAIC_wt") %in% names(dfs))) {
    idx1 <- grep("^WAIC$", names(dfs))
    idx2 <- grep("WAIC_wt", names(dfs))
    last_part <- (idx1 + 1):ncol(dfs)
    dfs <- dfs[, c(1:idx1, idx2, last_part[last_part != idx2])]
  }
  if (all(c("LOOIC", "LOOIC_wt") %in% names(dfs))) {
    idx1 <- grep("^LOOIC$", names(dfs))
    idx2 <- grep("LOOIC_wt", names(dfs))
    last_part <- (idx1 + 1):ncol(dfs)
    dfs <- dfs[, c(1:idx1, idx2, last_part[last_part != idx2])]
  }

  # for REML fits, warn user
  if (isTRUE(verbose) &&
    # only warn for REML fit
    identical(estimator, "REML") &&
    # only for IC comparison
    any(grepl("(AIC|BIC)", names(dfs))) &&
    # only when mixed models are involved, others probably don't have problems with REML fit
    any(sapply(objects, insight::is_mixed_model)) &&
    # only if not all models have same fixed effects (else, REML is ok)
    !isTRUE(attributes(objects)$same_fixef)) {
    warning(insight::format_message(
      "Information criteria (like AIC) are based on REML fits (i.e. `estimator=\"REML\"`).",
      "Please note that information criteria are probably not directly comparable and that it is not recommended comparing models with different fixed effects in such cases."
    ), call. = FALSE)
  }

  # dfs[order(sapply(object_names, as.character), dfs$Model), ]
  class(dfs) <- c("compare_performance", "see_compare_performance", class(dfs))
  dfs
}



# methods ----------------------------

#' @export
print.compare_performance <- function(x, digits = 3, ...) {
  table_caption <- c("# Comparison of Model Performance Indices", "blue")
  formatted_table <- format(x = x, digits = digits, format = "text", ...)

  if ("Performance_Score" %in% colnames(formatted_table)) {
    footer <- c(sprintf("\nModel %s (of class %s) performed best with an overall performance score of %s.", formatted_table$Model[1], formatted_table$Type[1], formatted_table$Performance_Score[1]), "yellow")
  } else {
    footer <- NULL
  }

  cat(insight::export_table(x = formatted_table, digits = digits, format = "text", caption = table_caption, footer = footer, ...))
  invisible(x)
}


#' @export
plot.compare_performance <- function(x, ...) {
  insight::check_if_installed("see", "for model comparison plots")
  NextMethod()
}



# utilities ------------------------------

.rank_performance_indices <- function(x, verbose) {
  # all models comparable?
  if (length(unique(x$Type)) > 1 && isTRUE(verbose)) {
    warning(insight::format_message("Models are not of same type. Comparison of indices might be not meaningful."), call. = FALSE)
  }

  # set reference for Bayes factors to 1
  if ("BF" %in% colnames(x)) x$BF[is.na(x$BF)] <- 1

  # don't include test statistic in ranking
  x$p_CochransQ <- NULL
  x$p_Omnibus <- NULL
  x$p <- NULL
  x$p_LRT <- NULL

  # use weights instead of information criteria
  x$AIC <- NULL
  x$AICc <- NULL
  x$BIC <- NULL
  x$LOOIC <- NULL
  x$WAIC <- NULL

  # remove extra columns from LOO criteria
  x$ELPD <- NULL
  x$ELPD_SE <- NULL
  x$LOOIC_SE <- NULL

  # don't rank with BF when there is also BIC (same information)
  if ("BF" %in% colnames(x) && "BIC_wt" %in% colnames(x)) {
    x$BF <- NULL
  }

  out <- x

  # normalize indices, for comparison
  out[] <- lapply(out, function(i) {
    if (is.numeric(i)) i <- .normalize_vector(i)
    i
  })

  # recode some indices, so higher values = better fit
  for (i in c("RMSE", "Sigma")) {
    if (i %in% colnames(out)) {
      out[[i]] <- 1 - out[[i]]
    }
  }

  # any indices with NA?
  missing_indices <- sapply(out, anyNA)
  if (any(missing_indices) && isTRUE(verbose)) {
    warning(insight::format_message(sprintf(
      "Following indices with missing values are not used for ranking: %s",
      paste0(colnames(out)[missing_indices], collapse = ", ")
    )), call. = FALSE)
  }

  # create rank-index, only for complete indices
  numeric_columns <- sapply(out, function(i) is.numeric(i) & !anyNA(i))
  rank_index <- rowMeans(out[numeric_columns], na.rm = TRUE)

  x$Performance_Score <- rank_index
  x <- x[order(rank_index, decreasing = TRUE), ]

  rownames(x) <- NULL
  x
}


.normalize_vector <- function(x) {
  as.vector((x - min(x, na.rm = TRUE)) / diff(range(x, na.rm = TRUE), na.rm = TRUE))
}


.ic_weight <- function(ic) {
  # ic should be in the deviance metric (-2 * loglik)
  if (is.null(ic)) {
    return(NULL)
  }

  diffs <- ic - min(ic)
  f <- exp(-0.5 * diffs)
  f / sum(f)
}
