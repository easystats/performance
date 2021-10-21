if (requiet("testthat") && requiet("performance")) {
  test_that("r2_tjur", {
    model <- glm(vs ~ wt + mpg, data = mtcars, family = "binomial")
    expect_equal(r2_tjur(model), c(`Tjur's R2` = 0.477692621360749), tolerance = 1e-3)
  })

  test_that("r2_tjur", {
    model <- lm(mpg ~ wt + cyl, data = mtcars)
    expect_error(r2_tjur(model))
  })
}
