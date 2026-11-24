test_that("check_symmetry returns p = 1 for perfectly symmetric data", {
  x <- c(-3, -2, -1, 0, 1, 2, 3)
  out <- check_symmetry(x)
  expect_equal(as.numeric(out), 1, tolerance = 1e-12)
})

test_that("check_symmetry uses Cabilio & Masaro (1996) variance and two-sided p-value", {
  x <- mtcars$mpg
  n <- length(x)
  z_expected <- sqrt(n / (pi / 2 - 1)) * (mean(x) - stats::median(x)) / stats::sd(x)
  p_expected <- 2 * stats::pnorm(abs(z_expected), lower.tail = FALSE)

  out <- check_symmetry(x)
  expect_equal(as.numeric(out), p_expected, tolerance = 1e-12)
})

test_that("check_symmetry detects skewed data and handles NAs", {
  set.seed(123)
  x_skew <- c(stats::rexp(200), NA)
  expect_lt(as.numeric(check_symmetry(x_skew)), 0.05)
})
