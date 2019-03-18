context("model_performance.bayesian")

test_that("model_performance.stanreg", {
  set.seed(333)

  library(rstanarm)
  library(circus)
  model <- circus::download_model("stanreg_lm_1")
  perf <- model_performance(model)

  testthat::expect_equal(perf$R2_Median, 0.751, tolerance = 0.01)
  testthat::expect_equal(perf$R2_LOO_adjusted, 0.7094, tolerance = 0.01)
  testthat::expect_equal(perf$ELPD, -83.514, tolerance = 0.01)

  model <- circus::download_model("stanreg_lm_2")
  perf <- model_performance(model)

  testthat::expect_equal(perf$R2_Median, 0.6392, tolerance = 0.01)
  testthat::expect_equal(perf$R2_LOO_adjusted, 0.587247, tolerance = 0.01)
  testthat::expect_equal(perf$ELPD, -31.622, tolerance = 0.01)

  model <- circus::download_model("stanreg_lmerMod_1")
  perf <- model_performance(model)

  testthat::expect_equal(perf$R2_Median, 0.6392, tolerance = 0.01)
  testthat::expect_equal(perf$R2_LOO_adjusted, 0.58724, tolerance = 0.01)
  testthat::expect_equal(perf$ELPD, -31.622, tolerance = 0.01)
})


test_that("model_performance.brmsfit", {
  testthat::skip_on_travis()
  set.seed(333)

  library(brms)

  model <- brms::brm(mpg ~ wt + cyl, data = mtcars)
  perf <- model_performance(model)
  testthat::expect_equal(perf$R2_Median, 0.826, tolerance = 0.01)
  testthat::expect_equal(perf$R2_LOO_adjusted, 0.791, tolerance = 0.01)
  testthat::expect_equal(perf$ELPD, -78.407, tolerance = 0.01)

  model <- brms::brm(Petal.Length ~ Petal.Width + (1 | Species), data = iris)
  perf <- model_performance(model)
  testthat::expect_equal(perf$R2_Median, 0.955, tolerance = 0.01)
  testthat::expect_equal(perf$R2_LOO_adjusted, 0.952, tolerance = 0.01)
  testthat::expect_equal(perf$ELPD, -70.5, tolerance = 0.01)
})
