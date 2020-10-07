if (require("testthat") && require("performance")) {
  test_that("model_performance.lm", {
    model <- lm(mpg ~ wt + cyl, data = mtcars)
    expect_equal(model_performance(model)$R2, 0.830, tolerance = 0.01)
    expect_equal(
      colnames(model_performance(model)),
      c("AIC", "BIC", "R2", "R2_adjusted", "RMSE", "Sigma")
    )
  })

  test_that("model_performance.glm", {
    model <- glm(vs ~ wt + mpg, data = mtcars, family = "binomial")
    expect_equal(model_performance(model)$R2_Tjur, 0.478, tolerance = 0.01)
    expect_equal(
      colnames(model_performance(model)),
      c("AIC", "BIC", "R2_Tjur", "RMSE", "Sigma", "Log_loss", "Score_log",
        "Score_spherical", "PCP")
    )
  })
}
