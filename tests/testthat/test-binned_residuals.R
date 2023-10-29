test_that("binned_residuals", {
  data(mtcars)
  model <- glm(vs ~ wt + mpg, data = mtcars, family = "binomial")
  result <- binned_residuals(model, ci_type = "gaussian", residuals = "response")
  expect_named(
    result,
    c("xbar", "ybar", "n", "x.lo", "x.hi", "se", "CI_low", "CI_high", "group")
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
  expect_equal(
    result$CI_low,
    c(-0.05686, -0.12331, -0.35077, -0.57683, 0.17916, -0.44147),
    tolerance = 1e-4
  )
})


test_that("binned_residuals, n_bins", {
  data(mtcars)
  model <- glm(vs ~ wt + mpg, data = mtcars, family = "binomial")
  result <- binned_residuals(model, ci_type = "gaussian", residuals = "response", n_bins = 10)
  expect_named(
    result,
    c("xbar", "ybar", "n", "x.lo", "x.hi", "se", "CI_low", "CI_high", "group")
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
  result <- binned_residuals(model, ci_type = "gaussian", residuals = "response", term = "mpg")
  expect_named(
    result,
    c("xbar", "ybar", "n", "x.lo", "x.hi", "se", "CI_low", "CI_high", "group")
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


test_that("binned_residuals, deviance residuals, gaussian CI", {
  data(mtcars)
  model <- glm(vs ~ wt + mpg, data = mtcars, family = "binomial")
  result <- binned_residuals(model, residuals = "deviance", ci_type = "gaussian")
  expect_named(
    result,
    c("xbar", "ybar", "n", "x.lo", "x.hi", "se", "CI_low", "CI_high", "group")
  )
  expect_equal(
    result$xbar,
    c(0.03786, 0.09514, 0.25911, 0.47955, 0.71109, 0.97119),
    tolerance = 1e-4
  )
  expect_equal(
    result$ybar,
    c(-0.26905, -0.44334, 0.03763, -0.19917, 0.81563, -0.23399),
    tolerance = 1e-4
  )
  expect_equal(
    result$ybar,
    c(-0.26905, -0.44334, 0.03763, -0.19917, 0.81563, -0.23399),
    tolerance = 1e-4
  )
  expect_equal(
    result$CI_low,
    c(-0.33985, -0.50865, -0.98255, -1.36025, 0.61749, -1.00913),
    tolerance = 1e-4
  )
})


test_that("binned_residuals, default", {
  data(mtcars)
  model <- glm(vs ~ wt + mpg, data = mtcars, family = "binomial")
  result <- binned_residuals(model)
  expect_named(
    result,
    c("xbar", "ybar", "n", "x.lo", "x.hi", "se", "CI_low", "CI_high", "group")
  )
  expect_equal(
    result$xbar,
    c(0.03786, 0.09514, 0.25911, 0.47955, 0.71109, 0.97119),
    tolerance = 1e-4
  )
  expect_equal(
    result$ybar,
    c(-0.26905, -0.44334, 0.03763, -0.19917, 0.81563, -0.23399),
    tolerance = 1e-4
  )
  expect_equal(
    result$CI_low,
    c(-0.52997, -0.70426, -0.32935, -0.59948, 0.55472, -0.55251),
    tolerance = 1e-4
  )
})


test_that("binned_residuals, bootstrapped CI", {
  skip_on_cran()
  data(mtcars)
  model <- glm(vs ~ wt + mpg, data = mtcars, family = "binomial")
  set.seed(123)
  result <- binned_residuals(model, ci_type = "boot", iterations = 100)
  expect_named(
    result,
    c("xbar", "ybar", "n", "x.lo", "x.hi", "se", "CI_low", "CI_high", "group")
  )
  expect_equal(
    result$xbar,
    c(0.03786, 0.09514, 0.25911, 0.47955, 0.71109, 0.97119),
    tolerance = 1e-4
  )
  expect_equal(
    result$ybar,
    c(-0.26905, -0.44334, 0.03763, -0.19917, 0.81563, -0.23399),
    tolerance = 1e-4
  )
  expect_equal(
    result$CI_low,
    c(-0.32623, -0.50543, -0.80879, -1.15154, 0.67569, -0.65748),
    tolerance = 1e-4
  )
})

test_that("binned_residuals, msg for non-bernoulli", {
  skip_on_cran()
  tot <- rep(10, 100)
  suc <- rbinom(100, prob = 0.9, size = tot)

  dat <- data.frame(tot, suc)
  dat$prop <- suc / tot
  dat$x1 <- as.factor(sample(1:5, 100, replace = TRUE))

  mod <- glm(prop ~ x1,
    family = binomial,
    data = dat,
    weights = tot
  )

  expect_message(binned_residuals(mod1), regex = "Using `ci_type = \"gaussian\"`")
  expect_silent(binned_residuals(mod1, verbose = FALSE))
})
