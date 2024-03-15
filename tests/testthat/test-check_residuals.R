test_that("check_singularity, lme4", {
  skip_on_cran()
  skip_if_not_installed("DHARMa")
  set.seed(123)
  dat <- DHARMa::createData(sampleSize = 100, overdispersion = 0.5, family = poisson())
  m <- glm(observedResponse ~ Environment1, family = poisson(), data = dat)
  res <- simulate_residuals(m)
  out <- check_residuals(res)
  expect_equal(out, 0.01884602, ignore_attr = TRUE, tolerance = 1e-4)
  expect_identical(
    capture.output(print(out)),
    "Warning: Non-uniformity of simulated residuals detected (p = 0.019)."
  )
  skip_if_not_installed("MASS")
  set.seed(3)
  mu <- rpois(500, lambda = 3)
  x <- rnorm(500, mu, mu * 3)
  x <- ceiling(x)
  x <- pmax(x, 0)
  quine.nb1 <- MASS::glm.nb(x ~ mu)
  set.seed(123)
  result <- check_residuals(quine.nb1)
  expect_equal(result, 0.000665414, tolerance = 1e-3)
})
