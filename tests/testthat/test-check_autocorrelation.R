test_that("check_autocorrelation", {
  data(mtcars)
  m <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
  out <- check_autocorrelation(m)
  expect_equal(out, 0.278, ignor_attr = TRUE, tolerance = 1e-3)
})
