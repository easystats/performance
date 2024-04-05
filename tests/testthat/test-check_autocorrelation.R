test_that("check_autocorrelation", {
  data(mtcars)
  m <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
  set.seed(123)
  out <- check_autocorrelation(m)
  expect_equal(as.vector(out), 0.316, ignore_attr = TRUE, tolerance = 1e-2)
  expect_identical(
    capture.output(print(out)),
    "OK: Residuals appear to be independent and not autocorrelated (p = 0.316)."
  )
  expect_warning(plot(out), "There is currently")
})
