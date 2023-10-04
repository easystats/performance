test_that("check_autocorrelation", {
  data(mtcars)
  m <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
  set.seed(123)
  out <- check_autocorrelation(m)
  expect_equal(as.vector(out), 0.316, ignor_attr = TRUE, tolerance = 1e-2)
})
