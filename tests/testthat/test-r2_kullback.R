if (require("testthat") && require("performance")) {
  test_that("r2_kullback", {
    model <- glm(vs ~ wt + mpg, data = mtcars, family = "binomial")
    expect_equal(r2_kullback(model), c(`Kullback-Leibler R2` = 0.383436177754933), tolerance = 1e-3)
    expect_equal(r2_kullback(model, adjust = FALSE), c(`Kullback-Leibler R2` = 0.423214488867518), tolerance = 1e-3)
  })
}
