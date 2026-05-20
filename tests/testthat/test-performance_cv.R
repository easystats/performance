test_that("performance_cv", {
  data(mtcars)
  model <- lm(mpg ~ wt + cyl, data = mtcars)

  set.seed(123)
  out <- performance_cv(model)
  expect_equal(out$MSE, 5.91153, tolerance = 1e-3)
  expect_named(out, c("MSE", "RMSE", "R2"))

  set.seed(123)
  out <- performance_cv(model, metrics = c("MSE", "R2"))
  expect_equal(out$MSE, 5.91153, tolerance = 1e-3)
  expect_named(out, c("MSE", "R2"))

  set.seed(123)
  out <- performance_cv(model, method = "loo")
  expect_equal(out$MSE, 7.376451, tolerance = 1e-3)

  set.seed(123)
  out <- performance_cv(model, method = "k_fold")
  expect_equal(out$MSE, 9.65578, tolerance = 1e-3)
})
