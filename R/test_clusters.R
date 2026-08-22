#' Test a clustering result against matched nulls
#'
#' `test_clusters()` compares the number of clusters selected from observed data
#' with the numbers selected from matched-null versions of those data. The null
#' data preserve the observed marginal distributions and correlation structure
#' but contain no cluster structure by construction. This complements
#' [`check_clusterstructure()`], which evaluates clustering tendency against a
#' spatial-uniformity null. The matched-null comparison is implemented with
#' [`matchednull::matched_null_test()`].
#'
#' @param x A numeric matrix or data frame with no missing or infinite values.
#' @param cluster_function A function that takes a numeric matrix and returns one
#'   number. By default, `mclust::Mclust()` selects the number of mixture
#'   components by BIC.
#' @param iterations Number of matched-null data sets to evaluate.
#' @param n_max Maximum number of clusters considered by the default
#'   `mclust::Mclust()` pipeline. Ignored when `cluster_function` is supplied.
#' @param standardize Logical. If `TRUE`, variables are standardized before the
#'   observed and matched-null cluster counts are computed.
#' @param ... Additional arguments passed to
#'   [`matchednull::matched_null_test()`], such as `copula`, `df`, `probs`,
#'   `ridge`, or `parallel`.
#'
#' @return An object of class `"test_clusters"` and `"matched_null_test"`. It
#'   contains the observed statistic (`real`), matched-null statistics (`null`),
#'   null interval (`interval`), one-sided Monte Carlo p-value (`p_exceed`), and
#'   interval verdict (`within`).
#'
#' @details
#' The default cluster-count pipeline fits Gaussian mixture models for one to
#' `n_max` components and returns the BIC-selected count. A custom
#' `cluster_function` receives the same preprocessed data as the default
#' pipeline and must return one non-missing number.
#'
#' The Gaussian matched null tests for structure beyond the observed margins
#' and correlations; rejection alone does not establish the existence of
#' discrete types. When tail dependence is plausible, rerun the test with
#' `copula = "t"` as a sensitivity analysis.
#'
#' @examplesIf requireNamespace("matchednull", quietly = TRUE) && requireNamespace("mclust", quietly = TRUE)
#' \donttest{
#' set.seed(42)
#' test_clusters(iris[, 1:4], iterations = 19, n_max = 4)
#'
#' # Any scalar-returning clustering pipeline can be tested.
#' pick_two <- function(data) 2
#' test_clusters(iris[, 1:4], cluster_function = pick_two, iterations = 19)
#' }
#'
#' @references
#' Meng, M. (2026). Types Without Taxa: A Covariance-Matched-Null Multiverse
#' Test of Categorical Versus Continuous Personality Structure. Manuscript
#' under review. \doi{10.17605/OSF.IO/2EKCG}
#'
#' @seealso [`matchednull::matched_null_test()`],
#'   [`check_clusterstructure()`]
#' @export
test_clusters <- function(
  x,
  cluster_function = NULL,
  iterations = 200,
  n_max = 10,
  standardize = TRUE,
  ...
) {
  insight::check_if_installed("matchednull", minimum_version = "0.2.1")

  if (!is.logical(standardize) || length(standardize) != 1L || is.na(standardize)) {
    insight::format_error("`standardize` must be `TRUE` or `FALSE`.")
  }
  if (!is.null(cluster_function) && !is.function(cluster_function)) {
    insight::format_error("`cluster_function` must be a function or `NULL`.")
  }
  uses_default <- is.null(cluster_function)
  if (!.is_positive_integer(iterations)) {
    insight::format_error("`iterations` must be a positive integer.")
  }
  if (uses_default && !.is_positive_integer(n_max)) {
    insight::format_error("`n_max` must be a positive integer.")
  }

  x <- .validate_test_clusters_data(x)
  x <- .standardize_test_clusters_data(x, standardize)

  if (uses_default) {
    insight::check_if_installed("mclust")
    mclustBIC <- mclust::mclustBIC
    max_components <- min(as.integer(n_max), nrow(x) - 1L)
    cluster_function <- function(data) {
      mclust::Mclust(
        data,
        G = seq_len(max_components),
        verbose = FALSE
      )$G
    }
  }

  out <- matchednull::matched_null_test(
    x,
    cluster_fn = cluster_function,
    R = as.integer(iterations),
    ...
  )

  attr(out, "standardize") <- standardize
  attr(out, "n_max") <- if (uses_default) max_components else NULL
  class(out) <- c("test_clusters", class(out))
  out
}


.validate_test_clusters_data <- function(x) {
  if (!is.matrix(x) && !is.data.frame(x)) {
    insight::format_error("`x` must be a numeric matrix or data frame.")
  }
  if (is.data.frame(x) && !all(vapply(x, is.numeric, logical(1)))) {
    insight::format_error("All columns in `x` must be numeric.")
  }

  x <- as.matrix(x)
  if (!is.numeric(x)) {
    insight::format_error("`x` must contain only numeric values.")
  }
  if (nrow(x) < 2L || ncol(x) < 1L) {
    insight::format_error("`x` must contain at least two rows and one column.")
  }
  if (anyNA(x) || !all(is.finite(x))) {
    insight::format_error("`x` must not contain missing or infinite values.")
  }
  x
}


.standardize_test_clusters_data <- function(x, standardize) {
  if (!standardize) {
    return(x)
  }

  column_sd <- apply(x, 2L, stats::sd)
  if (!all(is.finite(column_sd)) || any(column_sd == 0)) {
    insight::format_error("`x` cannot contain constant columns when `standardize = TRUE`.")
  }
  scale(x)
}


.is_positive_integer <- function(x) {
  is.numeric(x) &&
    length(x) == 1L &&
    !is.na(x) &&
    x >= 1L &&
    x %% 1 == 0
}
