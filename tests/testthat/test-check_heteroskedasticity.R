test_that("check_heteroskedasticity", {
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
