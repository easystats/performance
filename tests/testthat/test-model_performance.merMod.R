context("model_performance.merMod")

test_that("model_performance.merMod", {
  library(circus)

  model <- circus::download_model("lmerMod_1")
  testthat::expect_equal(model_performance(model)$AIC, 71.59892, tolerance = 0.01)

  model <- circus::download_model("merMod_1")
  testthat::expect_equal(model_performance(model)$AIC, 23.58593, tolerance = .01)

  model <- circus::download_model("merMod_2")
  testthat::expect_equal(model_performance(model)$AIC, 23.26831, tolerance = .01)
})
