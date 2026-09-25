test_that("test_clusters supports a custom cluster function", {
  skip_if_not_installed("matchednull", minimum_version = "0.2.1")

  set.seed(12)
  x <- matrix(rnorm(120), ncol = 3)
  out <- test_clusters(
    x,
    cluster_function = function(data) 2,
    iterations = 9
  )

  expect_s3_class(out, "test_clusters")
  expect_s3_class(out, "matched_null_test")
  expect_identical(out$real, 2)
  expect_length(out$null, 9)
  expect_true(out$within)
  expect_identical(attr(out, "outcome"), "within")
  expect_true(attr(out, "standardize"))
  expect_null(attr(out, "n_max"))
  expect_output(print(out), "Matched-null test of the cluster count")
  expect_output(print(out), "MC exceedance (twins >= observed):", fixed = TRUE)
  printed <- capture.output(print(out))
  p_line <- grep("MC exceedance", printed, value = TRUE, fixed = TRUE)
  p_shown <- as.numeric(sub(".*:\\s+", "", p_line))
  expect_equal(p_shown, (1 + sum(out$null >= out$real)) / (out$R + 1), tolerance = 5e-4)
  expect_output(print(out), "Gaussian copula, 9 matched-null twins", fixed = TRUE)
  expect_output(
    print(out),
    "No detected excess: the count lies within the reference interval.",
    fixed = TRUE
  )

  out_t <- test_clusters(
    x,
    cluster_function = function(data) 2,
    iterations = 3,
    copula = "t",
    df = 4
  )
  expect_identical(out_t$copula, "t")
  expect_identical(out_t$df, 4)
  expect_output(print(out_t), "t copula (df = 4), 3 matched-null twins", fixed = TRUE)
})


test_that("test_clusters reports counts above and below the reference range", {
  skip_if_not_installed("matchednull", minimum_version = "0.2.1")

  set.seed(5)
  x <- matrix(rnorm(120), ncol = 3)
  # matchednull evaluates the observed data first and the twins afterwards
  first_then <- function(first, rest) {
    calls <- 0
    function(data) {
      calls <<- calls + 1
      if (calls == 1) first else rest
    }
  }

  above <- test_clusters(x, cluster_function = first_then(5, 2), iterations = 9)
  expect_identical(above$real, 5)
  expect_false(above$within)
  expect_identical(attr(above, "outcome"), "above")
  expect_output(
    print(above),
    "Departure from the reference model: the count exceeds the interval.",
    fixed = TRUE
  )
  expect_output(
    print(above),
    "The test does not settle what kind of departure.",
    fixed = TRUE
  )

  below <- test_clusters(x, cluster_function = first_then(1, 2), iterations = 9)
  expect_identical(below$real, 1)
  expect_false(below$within)
  expect_identical(attr(below, "outcome"), "below")
  expect_output(
    print(below),
    "Below the reference range: a diagnostic signal, not a verdict.",
    fixed = TRUE
  )
})


test_that("test_clusters is reproducible from a seed", {
  skip_if_not_installed("matchednull", minimum_version = "0.2.1")

  set.seed(3)
  x <- matrix(rnorm(180), ncol = 3)
  cluster_summary <- function(data) sum(data[1:5, 1])

  set.seed(11)
  out1 <- test_clusters(x, cluster_summary, iterations = 9)
  set.seed(11)
  out2 <- test_clusters(x, cluster_summary, iterations = 9)

  expect_identical(out1$null, out2$null)
  expect_gt(stats::var(out1$null), 0)
})


test_that("default mclust pipeline distinguishes null and positive controls", {
  skip_if_not_installed("matchednull", minimum_version = "0.2.1")
  skip_if_not_installed("mclust")

  set.seed(42)
  x_null <- matrix(rnorm(150 * 4), 150, 4) %*%
    chol(diag(4) * 0.5 + 0.5)
  set.seed(7)
  null_result <- test_clusters(x_null, iterations = 9, n_max = 4)
  expect_true(null_result$within)
  expect_identical(attr(null_result, "outcome"), "within")
  expect_identical(attr(null_result, "n_max"), 4L)

  set.seed(42)
  group <- sample.int(2, 150, replace = TRUE)
  x_positive <- matrix(rnorm(150 * 4), 150, 4)
  positive_correlation <- chol(matrix(c(1, 0.9, 0.9, 1), 2, 2))
  negative_correlation <- chol(matrix(c(1, -0.9, -0.9, 1), 2, 2))
  x_positive[group == 1, 1:2] <- x_positive[group == 1, 1:2] %*% positive_correlation
  x_positive[group == 1, 3:4] <- x_positive[group == 1, 3:4] %*% positive_correlation
  x_positive[group == 2, 1:2] <- x_positive[group == 2, 1:2] %*% negative_correlation
  x_positive[group == 2, 3:4] <- x_positive[group == 2, 3:4] %*% negative_correlation

  set.seed(7)
  positive_result <- test_clusters(x_positive, iterations = 9, n_max = 4)
  expect_gt(positive_result$real, positive_result$interval[2])
  expect_identical(attr(positive_result, "outcome"), "above")
})


test_that("default mclust pipeline works in a clean session without attaching mclust", {
  # `mclust::Mclust()` evaluates `mclustBIC` in the caller's frame; this only
  # fails when mclust is not attached, so it has to be checked in a fresh
  # process against the installed package.
  skip_on_cran()
  skip_if_not_installed("matchednull", minimum_version = "0.2.1")
  skip_if_not_installed("mclust")

  # the child needs every library the parent sees, not only the first one:
  # dependencies such as insight often live in a different library path
  libs <- deparse1(.libPaths(), collapse = "")
  code <- paste(
    sprintf(".libPaths(c(%s, .libPaths()));", libs),
    "loaded <- tryCatch(",
    "  {",
    "    loadNamespace('performance');",
    "    TRUE",
    "  },",
    "  error = function(e) gsub('[\\r\\n]+', ' ', conditionMessage(e))",
    ");",
    "if (!isTRUE(loaded)) {",
    "  cat(paste0('LOADFAIL=', loaded, '\\n'))",
    "} else {",
    "  version <- as.character(utils::packageVersion('performance'));",
    "  if (!'test_clusters' %in% getNamespaceExports('performance')) {",
    "    cat(paste0('NOEXPORT;VERSION=', version, '\\n'))",
    "  } else {",
    "    set.seed(1);",
    "    x <- matrix(rnorm(180), 60, 3) %*% chol(diag(3) * 0.5 + 0.5);",
    "    out <- performance::test_clusters(x, iterations = 3, n_max = 3);",
    "    cat(paste0(",
    "      'ATTACHED=', 'package:mclust' %in% search(),",
    "      ';REAL=', out$real, ';VERSION=', version, '\\n'",
    "    ))",
    "  }",
    "}"
  )
  output <- system2(
    file.path(R.home("bin"), "Rscript"),
    c("--vanilla", "-e", shQuote(code)),
    stdout = TRUE,
    stderr = TRUE
  )
  result <- grep("^(LOADFAIL=|NOEXPORT;|ATTACHED=)", output, value = TRUE)
  reported <- paste(output, collapse = "\n")

  if (length(result) != 1L) {
    # neither a usable result nor a reason to skip: this must not pass quietly
    fail(paste0("Unexpected output from the clean session:\n", reported))
  } else if (startsWith(result, "NOEXPORT;")) {
    # an older performance is installed, so the fix cannot be checked here
    skip(paste0(
      "the installed performance (version ",
      sub("^NOEXPORT;VERSION=", "", result),
      ") does not export test_clusters()"
    ))
  } else if (startsWith(result, "LOADFAIL=")) {
    fail(paste0(
      "The clean session could not load performance:\n",
      reported
    ))
  } else {
    expect_match(result, "^ATTACHED=FALSE;REAL=")
    real <- suppressWarnings(as.numeric(
      sub("^ATTACHED=FALSE;REAL=([^;]+);VERSION=.*$", "\\1", result)
    ))
    expect_true(is.finite(real))
  }
})


test_that("test_clusters validates the raw return value of cluster_function", {
  skip_if_not_installed("matchednull", minimum_version = "0.2.1")

  set.seed(8)
  x <- matrix(rnorm(60), ncol = 2)
  returns <- function(value) function(data) value
  # error messages are wrapped to the console width, so match across line breaks
  not_finite <- "single\\s+finite\\s+number"

  expect_error(test_clusters(x, returns(NA_real_), iterations = 3), not_finite)
  expect_error(test_clusters(x, returns(Inf), iterations = 3), not_finite)
  expect_error(test_clusters(x, returns("2"), iterations = 3), not_finite)
  expect_error(test_clusters(x, returns(factor(2)), iterations = 3), not_finite)
  expect_error(test_clusters(x, returns(TRUE), iterations = 3), not_finite)
  expect_error(test_clusters(x, returns(c(2, 3)), iterations = 3), not_finite)
  expect_error(test_clusters(x, returns(NULL), iterations = 3), not_finite)
  expect_error(
    test_clusters(x, returns("2"), iterations = 3),
    "character\\s+of\\s+length\\s+1"
  )
  expect_error(
    test_clusters(x, returns(c(2, 3)), iterations = 3),
    "numeric\\s+of\\s+length\\s+2"
  )
  expect_error(
    test_clusters(x, returns(NULL), iterations = 3),
    "NULL\\s+of\\s+length\\s+0"
  )
  expect_error(test_clusters(x, returns(NA_real_), iterations = 3), "`NA`")

  # the check also runs on every twin, where matchednull would otherwise drop
  # a missing value from the exceedance probability
  calls <- 0
  na_on_twins <- function(data) {
    calls <<- calls + 1
    if (calls == 1) 2 else NA_real_
  }
  expect_error(test_clusters(x, na_on_twins, iterations = 3), not_finite)

  # errors raised inside cluster_function propagate unchanged
  calls <- 0
  fails_on_twin <- function(data) {
    calls <<- calls + 1
    if (calls > 1) {
      stop("twin failure", call. = FALSE)
    }
    2
  }
  expect_error(test_clusters(x, fails_on_twin, iterations = 3), "twin failure")

  # a length-one integer or 1x1 matrix is accepted and coerced to a plain number
  out <- test_clusters(x, returns(2L), iterations = 3)
  expect_identical(out$real, 2)
  out <- test_clusters(x, returns(matrix(2)), iterations = 3)
  expect_identical(out$null, c(2, 2, 2))
})


test_that("test_clusters validates its inputs", {
  skip_if_not_installed("matchednull", minimum_version = "0.2.1")

  x <- matrix(rnorm(40), ncol = 2)
  expect_error(test_clusters(1:10), "matrix or data frame")
  expect_error(test_clusters(data.frame(x = 1:3, y = letters[1:3])), "numeric")
  expect_error(test_clusters(cbind(x, NA_real_)), "missing or infinite")
  expect_error(test_clusters(cbind(x, 1)), "constant columns")
  expect_error(test_clusters(x, cluster_function = 1), "must be a function")
  expect_error(test_clusters(x, iterations = 1.5), "positive integer")
  expect_error(test_clusters(x, n_max = 0), "positive integer")
})
