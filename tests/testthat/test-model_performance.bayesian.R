context("model_performance.bayesian")

test_that("model_performance.stanreg", {
  set.seed(333)

  library(rstanarm)
  model <- rstanarm::stan_glm(mpg ~ wt + cyl, data = mtcars)
  perf <- model_performance(model)

  testthat::expect_equal(perf$R2_Median, 0.826, tolerance = 0.01)
  testthat::expect_equal(perf$R2_LOO_adj, 0.791, tolerance = 0.01)
  testthat::expect_equal(perf$ELPD, -78.407, tolerance = 0.01)

  model <- rstanarm::stan_lmer(mpg ~ wt + (1|gear), data = mtcars)
  perf <- model_performance(model)

  testthat::expect_equal(perf$R2_Median, 0.77, tolerance = 0.01)
  testthat::expect_equal(perf$R2_LOO_adj, 0.72, tolerance = 0.01)
  testthat::expect_equal(perf$ELPD, -82.88, tolerance = 0.01)
})