test_that("check_residuals and simulate_residuals", {
  skip_on_cran()
  skip_if_not_installed("DHARMa")
  set.seed(123)
  dat <- DHARMa::createData(sampleSize = 100, overdispersion = 0.5, family = poisson())
  m <- glm(observedResponse ~ Environment1, family = poisson(), data = dat)
  res <- simulate_residuals(m)
  expect_identical(
    capture.output(print(res)),
    c(
      "Simulated residuals from a model of class `glm` based on 250",
      "  simulations. Use `check_residuals()` to check uniformity of residuals or",
      "  `residuals()` to extract simulated residuals. It is recommended to refer",
      "  to `?DHARMa::simulateResiudals` and `vignette(\"DHARMa\")` for more",
      "  information about different settings in particular situations or for",
      "  particular models."
    )
  )
  # check raw residuals
  expect_equal(
    head(residuals(res)),
    c(0.55349, 0.44012, 0.39826, 0.9825, 0.90753, 0.05809),
    tolerance = 1e-4,
    ignore_attr = TRUE
  )
  expect_equal(
    head(residuals(res, quantile_function = stats::qnorm)),
    c(0.13448, -0.15068, -0.25785, 2.10826, 1.3257, -1.57097),
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
  expect_identical(sum(is.infinite(residuals(res, quantile_function = stats::qnorm))), 3L)
  expect_identical(sum(is.infinite(residuals(res, quantile_function = stats::qnorm, outlier_values = c(-100, 100)))), 0L) # nolint
  expect_error(residuals(res, quantile_function = stats::qnorm, outlier_values = 1:3), regex = "`outlier_values` must be") # nolint

  # check_residuals
  out <- check_residuals(res)
  expect_equal(out, 0.01884602, ignore_attr = TRUE, tolerance = 1e-4)
  expect_identical(
    capture.output(print(out)),
    "Warning: Non-uniformity of simulated residuals detected (p = 0.019)."
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
