test_that("check_residuals and simulate_residuals", {
  skip_on_cran()
  skip_if_not_installed("DHARMa", minimum_version = "0.5.2")
  set.seed(123)
  dat <- DHARMa::createData(sampleSize = 100, overdispersion = 0.5, family = poisson())
  m <- glm(observedResponse ~ Environment1, family = poisson(), data = dat)
  res <- simulate_residuals(m)
  # check raw residuals
  expect_equal(
    head(residuals(res)),
    c(0.01412, 0.53685, 0.43647, 0.04743, 0.32595, 0.13989),
    tolerance = 1e-4,
    ignore_attr = TRUE
  )
  expect_equal(
    head(residuals(res, quantile_function = stats::qnorm)),
    switch(
      snapshot_variant(),
      linux = c(-2.09365, 0.54232, 0.1529, -0.86756, -0.4839, -1.27504),
      c(-2.19387, 0.0925, -0.15993, -1.67034, -0.45112, -1.08081)
    ),
    tolerance = 1e-4,
    ignore_attr = TRUE
  )
  # compare to DHARMa
  res_d <- DHARMa::simulateResiduals(m, n = 250, plot = FALSE)
  expect_equal(
    head(residuals(res)),
    head(residuals(res_d)),
    tolerance = 1e-4,
    ignore_attr = TRUE
  )
  expect_equal(
    head(residuals(res, quantile_function = stats::qnorm)),
    head(residuals(res_d, quantileFunction = stats::qnorm)),
    tolerance = 1e-4,
    ignore_attr = TRUE
  )
  # DHARMa args work in residuals.permormance_simres
  expect_equal(
    residuals(res, quantileFunction = stats::qnorm, outlierValues = c(-3, 3)),
    residuals(res_d, quantileFunction = stats::qnorm, outlierValues = c(-3, 3)),
    tolerance = 1e-4,
    ignore_attr = TRUE
  )
  # outlier_values works
  expect_identical(sum(is.infinite(residuals(res, quantile_function = stats::qnorm))), 2L)
  expect_identical(
    sum(is.infinite(residuals(
      res,
      quantile_function = stats::qnorm,
      outlier_values = c(-100, 100)
    ))),
    0L
  )
  expect_error(
    residuals(res, quantile_function = stats::qnorm, outlier_values = 1:3),
    regex = "`outlier_values` must be"
  )

  # check_residuals
  out <- check_residuals(res)
  expect_equal(as.numeric(out), 0.00961, ignore_attr = TRUE, tolerance = 1e-4)
  expect_identical(
    capture.output(print(out)),
    "Warning: Non-uniformity of simulated residuals detected (p = 0.010)."
  )
  expect_error(simulate_residuals(m, iterations = 1), "`iterations` must be")

  skip_if_not_installed("MASS")
  set.seed(3)
  mu <- rpois(500, lambda = 3)
  x <- rnorm(500, mu, mu * 3)
  x <- ceiling(x)
  x <- pmax(x, 0)
  quine.nb1 <- MASS::glm.nb(x ~ mu)
  set.seed(123)
  result <- check_residuals(quine.nb1)
  expect_equal(result, 0.000665414, tolerance = 1e-3, ignore_attr = TRUE)
})
