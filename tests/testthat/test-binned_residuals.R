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


test_that("binned_residuals, n_bins", {
  data(mtcars)
  model <- glm(vs ~ wt + mpg, data = mtcars, family = "binomial")
  result <- binned_residuals(model, n_bins = 10)
  expect_named(
    result,
    c("xbar", "ybar", "n", "x.lo", "x.hi", "se", "ci_range", "CI_low", "CI_high", "group")
  )
  expect_equal(
    result$xbar,
    c(
      0.02373, 0.06301, 0.08441, 0.17907, 0.29225, 0.44073, 0.54951, 
      0.69701, 0.9168, 0.99204
    ),
    tolerance = 1e-4
  )
  expect_equal(
    result$ybar,
    c(
      -0.02373, -0.06301, -0.08441, -0.17907, 0.20775, -0.1074, 0.11715,
      0.30299, -0.25014, 0.00796
    ),
    tolerance = 1e-4
  )
})


test_that("binned_residuals, terms", {
  data(mtcars)
  model <- glm(vs ~ wt + mpg, data = mtcars, family = "binomial")
  result <- binned_residuals(model, term = "mpg")
  expect_named(
    result,
    c("xbar", "ybar", "n", "x.lo", "x.hi", "se", "ci_range", "CI_low", "CI_high", "group")
  )
  expect_equal(
    result$xbar,
    c(12.62, 15.34, 18.1, 20.9, 22.875, 30.06667),
    tolerance = 1e-4
  )
  expect_equal(
    result$ybar,
    c(-0.05435, -0.07866, 0.13925, -0.11861, 0.27763, -0.13786),
    tolerance = 1e-4
  )
})
