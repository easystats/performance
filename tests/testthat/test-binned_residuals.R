test_that("binned_residuals", {
  data(mtcars)
  model <- glm(vs ~ wt + mpg, data = mtcars, family = "binomial")
  result <- binned_residuals(model)
  expect_named(
    result,
    c("xbar", "ybar", "n", "x.lo", "x.hi", "se", "ci_range", "CI_low", "CI_high", "group")
  )
  expect_equal(
    result$xbar,
    c(0.03786, 0.09514, 0.25911, 0.47955, 0.71109, 0.97119),
    tolerance = 1e-4
  )
  expect_equal(
    result$ybar,
    c(-0.03786, -0.09514, 0.07423, -0.07955, 0.28891, -0.13786),
    tolerance = 1e-4
  )
})
