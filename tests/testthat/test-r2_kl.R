if (require("testthat") && require("performance")) {
  context("r2_kl")

  test_that("r2_kl", {
    model <- glm(vs ~ wt + mpg, data = mtcars, family = "binomial")
    expect_equal(r2_kl(model), c(`Kullback-Leibler R2` = 0.383436177754933), tolerance = 1e-3)
    expect_equal(r2_kl(model, adjust = FALSE), c(`Kullback-Leibler R2` = 0.423214488867518), tolerance = 1e-3)
  })
}