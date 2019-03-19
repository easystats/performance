context("cronbachs_alpha")

test_that("cronbachs_alpha", {
  data(mtcars)
  x <- mtcars[, c("cyl", "gear", "carb", "hp")]
  testthat::expect_equal(cronbachs_alpha(x), 0.09463, tol=0.01)
})
