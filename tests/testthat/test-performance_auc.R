test_that("performance_auc", {
  model_auc <- glm(vs ~ wt + mpg, data = mtcars, family = "binomial")
  # message
  set.seed(3)
  expect_message({
    out <- performance_accuracy(model_auc)
  })
  expect_equal(out$Accuracy, 0.75833, tolerance = 1e-3)
  expect_equal(out$CI_low, 0.6, tolerance = 1e-3)
  expect_equal(out$CI_high, 0.9875, tolerance = 1e-3)

  set.seed(12)
  expect_message({
    out <- performance_accuracy(model_auc)
  })
  expect_equal(out$Accuracy, 0.97222, tolerance = 1e-3)
  expect_equal(out$CI_low, 0.89722, tolerance = 1e-3)
  expect_equal(out$CI_high, 1, tolerance = 1e-3)

  # message
  set.seed(3)
  expect_message({
    out <- performance_accuracy(model, ci = 0.8)
  })
  expect_equal(out$Accuracy, 0.75833, tolerance = 1e-3)
  expect_equal(out$CI_low, 0.6, tolerance = 1e-3)
  expect_equal(out$CI_high, 0.95, tolerance = 1e-3)

  model_auc <- lm(mpg ~ wt + cyl, data = mtcars)
  set.seed(123)
  out <- performance_accuracy(model_auc)
  expect_equal(out$Accuracy, 0.94303, tolerance = 1e-3)
  expect_equal(out$CI_low, 0.8804, tolerance = 1e-3)
  expect_equal(out$CI_high, 0.98231, tolerance = 1e-3)

  set.seed(123)
  out <- performance_accuracy(model_auc, ci = 0.8)
  expect_equal(out$Accuracy, 0.94303, tolerance = 1e-3)
  expect_equal(out$CI_low, 0.90197, tolerance = 1e-3)
  expect_equal(out$CI_high, 0.97567, tolerance = 1e-3)
})
