if (require("testthat") && require("performance")) {
  test_that("r2_coxsnell", {
    model <- glm(vs ~ wt + mpg, data = mtcars, family = "binomial")
    expect_equal(r2_coxsnell(model), c(`Cox & Snell's R2` = 0.440140715155838), tolerance = 1e-3)
  })
}
