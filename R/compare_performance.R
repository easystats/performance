#' @title Compare performance of different models
#' @name compare_performance
#'
#' @description \code{compare_performance()} computes indices of model performance for
#' different models at once and hence allows comparison of indices across models.
#'
#' @param ... Multiple model objects (also of different classes).
#' @param metrics Can be \code{"all"}, \code{"common"} or a character vector of metrics to be computed. See related \code{\link[=model_performance]{documentation}} of object's class for details.
#' @param rank Logical, if \code{TRUE}, models are ranked according to 'best' overall model performance. See 'Details'.
#' @param bayesfactor Logical, if \code{TRUE}, a Bayes factor for model comparisons is possibly returned. See 'Details'.
#'
#' @return A data frame (with one row per model) and one column per "index" (see \code{metrics}).
#'
#' @note There is also a \href{https://easystats.github.io/see/articles/performance.html}{\code{plot()}-method} implemented in the \href{https://easystats.github.io/see/}{\pkg{see}-package}.
#'
#' @details \subsection{Bayes factor for Model Comparison}{
#'   If all models were fit from the same data, \code{compare_performance()}
#'   returns an additional column named \code{BF}, which shows the Bayes factor
#'   (see \code{bayestestR::bayesfactor_models()}) for each model against
#'   the denominator model. The \emph{first} model is used as denominator model,
#'   and its Bayes factor is set to \code{NA} to indicate the reference model.
#'   }
#'   \subsection{Ranking Models}{
#'   When \code{rank = TRUE}, a new column \code{Performance_Score} is returned. This
#'   score ranges from 0\% to 100\%, higher values indicating better model performance.
#'   Note that all score value do not necessarily sum up to 100\%. Rather,
#'   calculation is based on normalizing all indices (i.e. rescaling them to a
#'   range from 0 to 1), and taking the mean value of all indices for each model.
#'   This is a rather quick heuristic, but might be helpful as exploratory index.
#'   \cr \cr
#'   When comparing frequentist models, both BIC and Bayes factor can be available.
#'   In such cases, since Bayes factor and BIC hold the same information (i.e.
#'   \code{cor(-$BIC, BF, method = "spearman")} is 1), the Bayes factor is ignored
#'   for the performance-score during ranking.
#'   \cr \cr
#'   In particular when models are of different types (e.g. mixed models, classical
#'   linear models, logistic regression, ...), not all indices will be computed
#'   for each model. In case where an index can't be calculated for a specific
#'   model type, this model gets an \code{NA} value. All indices that have any
#'   \code{NA}s are excluded from calculating the performance score.
#'   \cr \cr
#'   There is a \code{plot()}-method for \code{compare_performance()},
#'   which creates a "spiderweb" plot, where the different indices are
#'   normalized and larger values indicate better model performance.
#'   Hence, points closer to the center indicate worse fit indices
#'   (see \href{https://easystats.github.io/see/articles/performance.html}{online-documentation}
#'   for more details).
#'   }
#'
#' @examples
#' if (require("lme4")) {
#'   m1 <- lm(mpg ~ wt + cyl, data = mtcars)
#'   m2 <- glm(vs ~ wt + mpg, data = mtcars, family = "binomial")
#'   m3 <- lmer(Petal.Length ~ Sepal.Length + (1 | Species), data = iris)
#'   compare_performance(m1, m2, m3)
#' }
#'
#' data(iris)
#' lm1 <- lm(Sepal.Length ~ Species, data = iris)
#' lm2 <- lm(Sepal.Length ~ Species + Petal.Length, data = iris)
#' lm3 <- lm(Sepal.Length ~ Species * Petal.Length, data = iris)
#' compare_performance(lm1, lm2, lm3)
#' compare_performance(lm1, lm2, lm3, rank = TRUE)
#' @importFrom insight is_model_supported all_models_equal get_response
#' @importFrom bayestestR bayesfactor_models
#' @inheritParams model_performance.lm
#' @export
compare_performance <- function(..., metrics = "all", rank = FALSE, bayesfactor = TRUE, verbose = TRUE) {
  objects <- list(...)
  object_names <- match.call(expand.dots = FALSE)$`...`

  supported_models <- sapply(objects, function(i) insight::is_model_supported(i) | inherits(i, "lavaan"))

  if (!all(supported_models)) {
    warning(sprintf("Following objects are not supported: %s", paste0(object_names[!supported_models], collapse = ", ")))
    objects <- objects[supported_models]
    object_names <- object_names[supported_models]
  }

  m <- mapply(function(.x, .y) {
    dat <- model_performance(.x, metrics = metrics, verbose = FALSE)
    model_name <- gsub("\"", "", .safe_deparse(.y), fixed = TRUE)
    cbind(data.frame(Model = model_name, Type = class(.x)[1], stringsAsFactors = FALSE), dat)
  }, objects, object_names, SIMPLIFY = FALSE)


  # check for identical model class, for bayesfactor
  if (isTRUE(bayesfactor)) {
    BFs <- tryCatch(
      {
        bayestestR::bayesfactor_models(..., denominator = 1, verbose = FALSE)
      },
      error = function(e) {
        NULL
      }
    )
  } else {
    BFs <- NULL
  }

  dfs <- Reduce(function(x, y) merge(x, y, all = TRUE, sort = FALSE), m)

  if (!is.null(BFs)) {
    dfs$BF <- BFs$BF
    dfs$BF[dfs$Model == object_names[1]] <- NA
  }

  # check if all models were fit from same data
  resps <- lapply(objects, insight::get_response)
  if (!all(sapply(resps[-1], function(x) identical(x, resps[[1]]))) && verbose) {
    warning("When comparing models, please note that probably not all models were fit from same data.", call. = FALSE)
  }

  # create "ranking" of models
  if (isTRUE(rank)) {
    dfs <- .rank_performance_indices(dfs, verbose)
  }

  # dfs[order(sapply(object_names, as.character), dfs$Model), ]
  class(dfs) <- c("compare_performance", "see_compare_performance", class(dfs))
  dfs
}




.rank_performance_indices <- function(x, verbose) {
  # all models comparable?
  if (length(unique(x$Type)) > 1 && isTRUE(verbose)) {
    warning("Models are not of same type. Comparison of indices might be not meaningful.", call. = FALSE)
  }

  # set reference for Bayes factors to 1
  if ("BF" %in% colnames(x)) x$BF[is.na(x$BF)] <- 1

  # don't include test statistic in ranking
  x$p_CochransQ <- NULL
  x$p_Omnibus <- NULL

  out <- x

  # normalize indices, for comparison
  out[] <- lapply(out, function(i) {
    if (is.numeric(i)) i <- .normalize_vector(i)
    i
  })

  # don't rank with BF when there is also BIC (same information)
  if ("BF" %in% colnames(out) && "BIC" %in% colnames(out)) {
    if (isTRUE(verbose)) {
      message("Bayes factor is based on BIC approximation, thus BF and BIC hold the same information. Ignoring BF for performance-score.")
    }
    out$BF <- NULL
  }

  # recode some indices, so higher values = better fit
  for (i in c("AIC", "BIC", "RMSE", "Sigma")) {
    if (i %in% colnames(out)) {
      out[[i]] <- 1 - out[[i]]
    }
  }

  # any indices with NA?
  missing_indices <- sapply(out, anyNA)
  if (any(missing_indices) && isTRUE(verbose)) {
    warning(sprintf(
      "Following indices with missing values are not used for ranking: %s",
      paste0(colnames(out)[missing_indices], collapse = ", ")
    ), call. = FALSE)
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
