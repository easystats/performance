context("model_performance.bayesian")

test_that("model_performance.stanreg", {
  set.seed(333)

  library(rstanarm)
  library(circus)
  model <- circus::download_model("stanreg_lm_1")
  perf <- model_performance(model)

  testthat::expect_equal(perf$R2_Median, 0.826, tolerance = 0.01)
  testthat::expect_equal(perf$R2_LOO_adj, 0.791, tolerance = 0.01)
  testthat::expect_equal(perf$ELPD, -78.407, tolerance = 0.01)

  model <- circus::download_model("stanreg_lm_2")
  testthat::expect_error(model_performance(model))  # Needs to be fixed

  model <- circus::download_model("stanreg_lmerMod_1")
  perf <- model_performance(model)

  testthat::expect_equal(perf$R2_Median, 0.64, tolerance = 0.01)
  testthat::expect_equal(perf$R2_LOO_adj, 0.5885, tolerance = 0.01)
  testthat::expect_equal(perf$ELPD, -31.590, tolerance = 0.01)
})