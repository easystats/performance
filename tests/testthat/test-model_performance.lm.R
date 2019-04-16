if (require("testthat") && require("performance")) {
  context("model_performance.lm")

  test_that("model_performance.lm", {
    model <- lm(mpg ~ wt + cyl, data = mtcars)
    expect_equal(model_performance(model)$R2, 0.830, tolerance = 0.01)

    model <- glm(vs ~ wt + mpg, data = mtcars, family = "binomial")
    expect_equal(model_performance(model)$R2_Tjur, 0.478, tolerance = 0.01)
  })
}