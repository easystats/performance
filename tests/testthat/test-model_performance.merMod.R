context("model_performance.merMod")

test_that("model_performance.merMod", {
  library(circus)

  model <- circus::download_model("lmerMod_1")
  testthat::expect_equal(model_performance(model)$AIC, 71.6, tolerance = 0.01)

  model <- circus::download_model("merMod_1")
  testthat::expect_error(model_performance(model)$AIC) # To be fixed

  model <- circus::download_model("merMod_2")
  testthat::expect_error(model_performance(model)$AIC) # To be fixed
})