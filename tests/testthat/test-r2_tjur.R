context("r2_tjur")

test_that("r2_tjur", {
  model <- glm(vs ~ wt + mpg, data = mtcars, family = "binomial")
  testthat::expect_equal(r2_tjur(model), 0.477, tolerance = 0.2)
})
