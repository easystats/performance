.runThisTest <- Sys.getenv("RunAllperformanceTests") == "yes"

if (require("testthat") && require("performance") && require("rstanarm") && require("brms")) {
  if (.runThisTest) {
    test_that("model_performance.stanreg", {
      set.seed(333)
      model <- insight::download_model("stanreg_lm_1")
      perf <- model_performance(model)

      expect_equal(perf$R2, 0.7398733, tolerance = 1e-3)
      expect_equal(perf$R2_adjusted, 0.7075685, tolerance = 1e-3)
      expect_equal(perf$ELPD, -83.49838, tolerance = 1e-3)

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

      model <- insight::download_model("brms_1")
      perf <- model_performance(model)
      expect_equal(perf$R2, 0.8262673, tolerance = 1e-3)
      expect_equal(perf$R2_adjusted, 0.7901985, tolerance = 1e-3)
      expect_equal(perf$ELPD, -78.59823, tolerance = 1e-3)

      model <- insight::download_model("brms_mixed_4")
      perf <- model_performance(model)
      expect_equal(perf$R2, 0.954538, tolerance = 1e-3)
      expect_equal(perf$R2_adjusted, 0.9523081, tolerance = 1e-3)
      expect_equal(perf$ELPD, -70.40493, tolerance = 1e-3)

      model <- insight::download_model("brms_ordinal_1")
      perf <- model_performance(model)
      expect_equal(perf$R2, 0.8760015, tolerance = 1e-3)
      expect_equal(perf$ELPD, -11.65433, tolerance = 1e-3)
    })
  }
}
