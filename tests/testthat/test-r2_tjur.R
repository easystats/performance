context("r2_tjur")

test_that("r2_tjur", {
  model <- glm(vs ~ wt + mpg, data = mtcars, family = "binomial")
  testthat::expect_equal(r2_tjur(model), c(`Tjur's R2` = 0.477692621360749), tolerance = 1e-3)
})
