#' Test a cluster count against matched-null data
#'
#' `test_clusters()` runs a cluster-count selector on the observed data and on
#' `iterations` matched-null twins of those data, and compares the observed
#' count with the counts the twins produce. The twins keep the observed values
#' of every variable exactly and reproduce the observed correlations
#' approximately, but contain no cluster structure by construction. The test
#' reports one of three outcomes: no detected excess, a departure from the
#' reference model, or a count below the reference range. The comparison is
#' implemented with [`matchednull::matched_null_test()`]. It asks a different
#' question from [`check_clusterstructure()`], which evaluates clustering
#' tendency against a spatial-uniformity null.
#'
#' @param x A numeric matrix or data frame with no missing or infinite values.
#' @param cluster_function A function that takes a numeric matrix and returns a
#'   single finite number, typically the selected number of clusters. By
#'   default, `mclust::Mclust()` selects the number of mixture components by
#'   BIC. The function is called once on the observed data and once on every
#'   twin, and its return value is checked each time (see 'Details').
#' @param iterations Number of matched-null twins to evaluate.
#' @param n_max Maximum number of clusters considered by the default
#'   `mclust::Mclust()` pipeline. Ignored when `cluster_function` is supplied.
#' @param standardize Logical. If `TRUE`, variables are standardized before the
#'   observed and matched-null cluster counts are computed.
#' @param ... Additional arguments passed to
#'   [`matchednull::matched_null_test()`], such as `copula`, `df`, `probs`,
#'   `ridge`, or `parallel`.
#'
#' @return An object of class `"test_clusters"` and `"matched_null_test"`: a
#'   list with the observed statistic (`real`), the twin statistics (`null`),
#'   the reference interval (`interval`), the Monte Carlo exceedance
#'   probability (`p_exceed`), the interval verdict (`within`), the number of
#'   twins (`R`), and the copula family (`copula`, with `df` for the t copula).
#'   The attribute `"outcome"` is `"within"`, `"above"`, or `"below"`: the
#'   position of the observed statistic relative to the reference interval.
#'   The attributes `"standardize"` and `"n_max"` record the preprocessing and
#'   the maximum number of components of the default pipeline (`n_max` is
#'   `NULL` when `cluster_function` is supplied).
#'
#' @details
#' The default cluster-count pipeline fits Gaussian mixture models with one to
#' `n_max` components using `mclust::Mclust()` and returns the BIC-selected
#' number of components. A user-supplied `cluster_function` receives the same
#' preprocessed data matrix as the default pipeline. Its return value is
#' checked on the observed data and on every twin: it must be a single finite
#' number, and any other value (`NA`, a character string, a factor, a logical
#' value, a vector of length two, `NULL`) is an error. The check runs here so
#' that invalid values are rejected whatever matchednull version is installed:
#' matchednull 0.2.1 dropped missing twin values from the exceedance
#' probability without notice, and later versions stop with an error of their
#' own.
#'
#' ## Reference model
#'
#' Each twin is drawn from a Gaussian copula whose latent correlation matrix
#' is the observed correlation matrix. Every column of a twin contains exactly
#' the observed values of that variable, rearranged in the rank order of the
#' copula draw. The margins are therefore reproduced exactly, but the
#' correlations only approximately: non-normal margins change the Pearson
#' correlations of the output relative to the latent correlation matrix. With
#' `copula = "t"`, the draw comes from a t copula instead, a sensitivity
#' variant with heavier-tailed dependence. Rerunning with `copula = "t"` shows
#' whether the outcome changes under that reference model; it does not by
#' itself establish or rule out clusters.
#'
#' ## Outcomes
#'
#' The observed count is compared with the interval between the `probs`
#' quantiles of the twin counts (by default the 2.5% and 97.5% quantiles).
#'
#' - A count above the interval is a departure from the specified reference
#'   model. The test does not settle what kind of departure it is, and the
#'   result is not evidence that clusters exist: typeless data with joint
#'   asymmetry, for example, can exceed the reference model as well.
#' - A count inside the interval is no detected excess for the statistic that
#'   was compared, the cluster count. It is not evidence that there are no
#'   clusters, and it does not establish that the joint distribution belongs
#'   to the reference family.
#' - A count below the interval is a diagnostic signal, not a verdict. It
#'   calls for checking the pipeline and the reference before any
#'   interpretation: whether the fits converged, whether the twins reproduce
#'   the intended margins and correlations, and whether data and twins were
#'   compared at the same sample size with the same statistic.
#'
#' `p_exceed` is a Monte Carlo exceedance probability under a reference model
#' fitted to the same data. It equals `(1 + k) / (iterations + 1)`, where `k`
#' is the number of twins whose count is at least the observed count. It is
#' not an exact finite-sample p-value.
#'
#' ## Before running the test
#'
#' Inspect the margins for pronounced multimodality first, for example with
#' [`check_multimodal()`] on single variables. Groups that differ only in
#' their means are detected at moderate separations. At large separations the
#' separation is expressed in the margins themselves, and the twins preserve
#' the margins exactly, so the reference model can reproduce such groups and
#' the comparison may then report no detected excess. This behaviour was
#' observed in the simulation constructions of Meng (2026); it is not asserted
#' as a general rule. Whether the joint distribution has clustering tendency is a different
#' question again; see [`check_clusterstructure()`], which uses a
#' spatial-uniformity null.
#'
#' @examplesIf all(insight::check_if_installed(c("matchednull", "mclust", "cluster"), quietly = TRUE))
#' \donttest{
#' set.seed(42)
#' test_clusters(iris[, 1:4], iterations = 19, n_max = 4)
#'
#' # Any pipeline that returns one number can be tested, for example k-means
#' # with the number of clusters chosen by the mean silhouette width.
#' select_k <- function(data) {
#'   ks <- 2:5
#'   sil <- vapply(ks, function(k) {
#'     cl <- stats::kmeans(data, centers = k, nstart = 5)$cluster
#'     mean(cluster::silhouette(cl, stats::dist(data))[, "sil_width"])
#'   }, numeric(1))
#'   ks[which.max(sil)]
#' }
#' test_clusters(iris[, 1:4], cluster_function = select_k, iterations = 19)
#' }
#'
#' @references
#' - Meng, M. (2026). matchednull: Matched-null tests for cluster-count claims.
#'   R package version 0.2.1. <https://CRAN.R-project.org/package=matchednull>
#'
#' - Meng, M. (2026). Types Without Taxa: A Covariance-Matched-Null Multiverse
#'   Test of Categorical Versus Continuous Personality Structure.
#'   Preregistration, Open Science Framework. \doi{10.17605/OSF.IO/2EKCG}
#'
#' @seealso [`matchednull::matched_null_test()`],
#'   [`check_clusterstructure()`], [`check_multimodal()`]
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
    # `mclust::Mclust()` rewrites its call to `mclustBIC` and evaluates it in
    # the caller's frame, so the symbol must be visible from this function
    # when mclust is not attached. Do not remove.
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
    cluster_fn = .checked_cluster_function(cluster_function),
    R = as.integer(iterations),
    ...
  )

  attr(out, "standardize") <- standardize
  attr(out, "n_max") <- if (uses_default) max_components else NULL
  attr(out, "outcome") <- .test_clusters_outcome(out$real, out$interval)
  class(out) <- c("test_clusters", class(out))
  out
}


#' @export
print.test_clusters <- function(x, digits = 3, ...) {
  outcome <- .test_clusters_outcome(x$real, x$interval)
  outcome_text <- switch(
    outcome,
    within = "No detected excess: the count lies within the reference interval.",
    above = paste0(
      "Departure from the reference model: the count exceeds the interval.\n",
      "The test does not settle what kind of departure."
    ),
    below = "Below the reference range: a diagnostic signal, not a verdict."
  )
  outcome_color <- switch(outcome, within = "green", above = "yellow", below = "yellow")

  reference <- if (is.null(x$copula) || x$copula == "gaussian") {
    "Gaussian copula"
  } else {
    sprintf(
      "t copula (df = %s)",
      insight::format_value(x$df, digits = digits, protect_integers = TRUE)
    )
  }

  interval <- sprintf(
    "[%s]",
    toString(insight::format_value(x$interval, digits = digits, protect_integers = TRUE))
  )
  if (!is.null(names(x$interval))) {
    interval <- sprintf(
      "%s (%s quantiles of the twins)",
      interval,
      paste(names(x$interval), collapse = " and ")
    )
  }

  insight::print_color("# Matched-null test of the cluster count\n\n", "blue")
  # the exceedance counts twins whose statistic is at least the observed one
  cat(sprintf(
    "  Observed statistic:                 %s\n",
    insight::format_value(x$real, digits = digits, protect_integers = TRUE)
  ))
  cat(sprintf("  Reference interval:                 %s\n", interval))
  cat(sprintf(
    "  MC exceedance (twins >= observed):  %s\n",
    insight::format_value(x$p_exceed, digits = digits)
  ))
  cat(sprintf(
    "  Reference model:                    %s, %s matched-null twins\n",
    reference,
    insight::format_value(x$R, protect_integers = TRUE)
  ))
  cat("\n")
  insight::print_color(paste0(outcome_text, "\n"), outcome_color)
  invisible(x)
}


# matchednull calls the clustering function once on the observed data and once
# per twin. matchednull 0.2.1 dropped missing twin values from the exceedance
# probability without notice, so the wrapper checks the raw return value
# itself, before any coercion, whatever version is installed.
.checked_cluster_function <- function(cluster_function) {
  function(data) {
    value <- cluster_function(data)
    if (!is.numeric(value) || length(value) != 1L || !is.finite(value)) {
      insight::format_error(sprintf(
        "`cluster_function` must return a single finite number, but returned %s.",
        .describe_cluster_value(value)
      ))
    }
    unname(as.numeric(value))
  }
}


.describe_cluster_value <- function(value) {
  if (is.numeric(value) && length(value) == 1L) {
    # a number that is not finite: NA, NaN, Inf or -Inf
    return(paste0("`", format(value), "`"))
  }
  sprintf("%s of length %d", class(value)[1], length(value))
}


.test_clusters_outcome <- function(real, interval) {
  if (real > interval[2]) {
    "above"
  } else if (real < interval[1]) {
    "below"
  } else {
    "within"
  }
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
    insight::format_error(
      "`x` cannot contain constant columns when `standardize = TRUE`."
    )
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
