if (requiet("testthat") && requiet("performance")) {
  model <- lm(mpg ~ wt + cyl, data = mtcars)

  set.seed(123)
  out <- performance_cv(model)

  test_that("performance_cv", {
    expect_equal(out$MSE, 5.91153, tolerance = 1e-3)
    expect_equal(colnames(out), c("MSE", "RMSE", "R2"), tolerance = 1e-3)
  })

  set.seed(123)
  out <- performance_cv(model, metrics = c("MSE", "R2"))

  test_that("performance_cv", {
    expect_equal(out$MSE, 5.91153, tolerance = 1e-3)
    expect_equal(colnames(out), c("MSE", "R2"), tolerance = 1e-3)
  })
}
