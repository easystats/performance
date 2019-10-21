.runThisTest <- Sys.getenv("RunAllinsightTests") == "yes"

if (require("testthat") && require("performance")) {
  if (.runThisTest && Sys.getenv("USER") != "travis") {
    context("model_performance.bayesian")

    test_that("model_performance.stanreg", {
      set.seed(333)
      library(rstanarm)

      model <- insight::download_model("stanreg_lm_1")
      perf <- model_performance(model)

      expect_equal(perf$R2, 0.7381938, tolerance = 1e-3)
      expect_equal(perf$R2_adjusted,  0.7110354, tolerance = 1e-3)
      expect_equal(perf$ELPD, -83.40375, tolerance = 1e-3)

      model <- insight::download_model("stanreg_lm_2")
      perf <- model_performance(model)

      expect_equal(perf$R2, 0.8168386, tolerance = 1e-3)
      expect_equal(perf$R2_adjusted, 0.791236, tolerance = 1e-3)
      expect_equal(perf$ELPD, -78.38735, tolerance = 1e-3)

      model <- insight::download_model("stanreg_lmerMod_1")
      perf <- model_performance(model)

      expect_equal(perf$R2, 0.6286546, tolerance = 1e-3)
      expect_equal(perf$R2_adjusted, 0.5896637, tolerance = 1e-3)
      expect_equal(perf$ELPD, -31.55849, tolerance = 1e-3)
    })


    test_that("model_performance.brmsfit", {
      set.seed(333)
      library(brms)

      model <- insight::download_model("brms_1")
      perf <- model_performance(model)
      expect_equal(perf$R2, 0.8267671, tolerance = 1e-3)
      expect_equal(perf$R2_adjusted, 0.7908548, tolerance = 1e-3)
      expect_equal(perf$ELPD, -78.43708, tolerance = 1e-3)

      model <- insight::download_model("brms_mixed_4")
      perf <- model_performance(model)
      expect_equal(perf$R2, 0.9545026, tolerance = 1e-3)
      expect_equal(perf$R2_adjusted, 0.9524251, tolerance = 1e-3)
      expect_equal(perf$ELPD, -70.2357, tolerance = 1e-3)

      model <- insight::download_model("brms_ordinal_1")
      perf <- model_performance(model)
      expect_equal(perf$R2, 0.873797, tolerance = 1e-3)
      expect_equal(perf$ELPD, -12.78422, tolerance = 1e-3)
    })

  }
}