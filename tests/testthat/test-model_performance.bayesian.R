.runThisTest <- Sys.getenv("RunAllinsightTests") == "yes"

if (require("testthat") && require("performance")) {
  if (.runThisTest && Sys.getenv("USER") != "travis") {
    context("model_performance.bayesian")

    test_that("model_performance.stanreg", {
      set.seed(333)
      library(rstanarm)

      model <- insight::download_model("stanreg_lm_1")
      perf <- model_performance(model)

      expect_equal(perf$R2, 0.7423714, tolerance = 1e-4)
      expect_equal(perf$R2_LOO_adjusted,  0.7110354, tolerance = 1e-4)
      expect_equal(perf$ELPD, -83.40375, tolerance = 1e-4)

      model <- insight::download_model("stanreg_lm_2")
      perf <- model_performance(model)

      expect_equal(perf$R2, 0.81877, tolerance = 0.01)
      expect_equal(perf$R2_LOO_adjusted, 0.791236, tolerance = 0.01)
      expect_equal(perf$ELPD, -78.38735, tolerance = 0.01)

      model <- insight::download_model("stanreg_lmerMod_1")
      perf <- model_performance(model)

      expect_equal(perf$R2, 0.6256033, tolerance = 0.01)
      expect_equal(perf$R2_LOO_adjusted, 0.5896637, tolerance = 0.01)
      expect_equal(perf$ELPD, -31.55849, tolerance = 0.01)
    })


    test_that("model_performance.brmsfit", {
      set.seed(333)
      library(brms)

      model <- insight::download_model("brms_1")
      perf <- model_performance(model)
      expect_equal(perf$R2, 0.8187403, tolerance = 0.01)
      expect_equal(perf$R2_LOO_adjusted, 0.7908548, tolerance = 0.01)
      expect_equal(perf$ELPD, -78.43981, tolerance = 0.01)

      model <- insight::download_model("brms_mixed_4")
      perf <- model_performance(model)
      expect_equal(perf$R2, 0.9541836, tolerance = 0.01)
      expect_equal(perf$R2_LOO_adjusted, 0.9524251, tolerance = 0.01)
      expect_equal(perf$ELPD, -70.23604, tolerance = 0.01)

      model <- insight::download_model("brms_ordinal_1")
      perf <- model_performance(model)
      expect_equal(perf$R2, 0.8702333, tolerance = 0.01)
      expect_equal(perf$ELPD, -12.77548, tolerance = 0.01)
    })

  }
}