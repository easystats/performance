test_that("check_heteroskedasticity, lm", {
  data(mtcars)
  m <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
  out <- check_heteroscedasticity(m)
  expect_equal(as.vector(out), 0.0423, ignore_attr = TRUE, tolerance = 1e-2)
  expect_identical(
    capture.output(print(out)),
    "Warning: Heteroscedasticity (non-constant error variance) detected (p = 0.042)."
  )
  m <- lm(mpg ~ hp, data = mtcars)
  out <- check_heteroscedasticity(m)
  expect_equal(as.vector(out), 0.8271352, ignore_attr = TRUE, tolerance = 1e-2)
  expect_identical(
    capture.output(print(out)),
    "OK: Error variance appears to be homoscedastic (p = 0.827)."
  )
})

test_that("check_heteroskedasticity, hlm", {
  set.seed(1)
  n <- 600
  size <- 20
  x <- runif(n, -3, 3)
  d <- data.frame(x = x, y = rbinom(n, size, plogis(-0.5 + 1.2 * x)))
  d$f <- size - d$y

  m <- glm(cbind(y, f) ~ x, family = binomial, data = d)
  expect_message(
    {
      out <- check_heteroscedasticity(m)
    },
    regex = "There is only a `plot()` method",
    fixed = TRUE
  )

  skip_if_not_installed("glmmTMB")
  m <- glmmTMB::glmmTMB(cbind(y, f) ~ x, family = binomial, data = d)
  expect_message(
    {
      out <- check_heteroscedasticity(m)
    },
    regex = "There is only a `plot()` method",
    fixed = TRUE
  )
})
