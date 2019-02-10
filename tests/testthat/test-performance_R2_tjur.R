context("performance_R2_tjur")

test_that("performance_R2_tjur", {
  model <- glm(vs ~ wt + mpg, data = mtcars, family = "binomial")
  testthat::expect_equal(performance_R2_tjur(model), 0.477, tolerance = 0.2)
})