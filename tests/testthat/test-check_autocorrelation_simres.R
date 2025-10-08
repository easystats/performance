test_that("check_autocorrelation works with simulated residuals", {
  skip_if_not_installed("DHARMa")
  skip_if_not_installed("glmmTMB")
  skip_if_not(getRversion() >= "4.0.0")

  data(Salamanders, package = "glmmTMB")

  # Test with a simple Poisson GLM
  m <- glm(count ~ spp + mined, family = poisson, data = Salamanders)

  # Simulate residuals
  set.seed(123)
  simres <- simulate_residuals(m)

  # Check autocorrelation
  set.seed(123)
  expect_warning({
    out <- check_autocorrelation(simres)
  })

  # Should return a p-value
  expect_type(out, "double")
  expect_s3_class(out, "check_autocorrelation")

  # P-value should be between 0 and 1
  expect_true(out >= 0 && out <= 1)
})


test_that("check_autocorrelation.DHARMa works", {
  skip_if_not_installed("DHARMa")

  # Test that the DHARMa method works
  data(mtcars)
  m <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)

  set.seed(123)
  simres <- DHARMa::simulateResiduals(m, plot = FALSE)

  expect_warning(check_autocorrelation(simres), regex = "Data are assumed")
  set.seed(123)
  expect_silent({
    out <- check_autocorrelation(simres, time = seq_along(simres$scaledResiduals))
  })

  # Should return a p-value
  expect_type(out, "double")
  expect_s3_class(out, "check_autocorrelation")

  expect_equal(as.vector(out), 0.4163168, tolerance = 1e-3, ignore_attr = TRUE)
})
