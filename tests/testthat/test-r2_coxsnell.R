context("r2_coxnell")

test_that("r2_coxnell", {
  model <- glm(vs ~ wt + mpg, data = mtcars, family = "binomial")
  testthat::expect_equal(r2_coxnell(model), c(`Cox & Snell's R2` = 0.440140715155838), tolerance = 1e-3)
})
