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
  expect_true(attr(out, "standardize"))
  expect_null(attr(out, "n_max"))
  expect_output(print(out), "Matched-null test")

  out_t <- test_clusters(
    x,
    cluster_function = function(data) 2,
    iterations = 3,
    copula = "t",
    df = 4
  )
  expect_identical(out_t$copula, "t")
  expect_identical(out_t$df, 4)
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
