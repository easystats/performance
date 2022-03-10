if (requiet("testthat") && requiet("insight") && requiet("performance") && requiet("httr") && requiet("lme4")) {
  .runThisTest <- Sys.getenv("RunAllperformanceTests") == "yes"

  if (.runThisTest) {
    test_that("model_performance.merMod", {
      model <- insight::download_model("lmerMod_1")
      expect_equal(model_performance(model)$AIC, AIC(logLik(model, REML = FALSE)), tolerance = 0.01)
      expect_equal(model_performance(model, estimator = "REML")$AIC, AIC(model), tolerance = 0.01)

      model <- insight::download_model("merMod_1")
      expect_equal(model_performance(model)$AIC, AIC(model), tolerance = 0.01)
      expect_equal(model_performance(model, estimator = "REML")$AIC, AIC(model), tolerance = 0.01)
      expect_equal(model_performance(model)$AIC, 23.58593, tolerance = .01)

      model <- insight::download_model("merMod_2")
      expect_equal(model_performance(model)$AIC, 21.4729, tolerance = .01)
    })


    test_that("model_performance.merMod AICc", {
      m1 <- lmer(mpg ~ disp + am + (1 | cyl), data = mtcars)
      m2 <- lm(mpg ~ hp + vs, data = mtcars)

      # REML
      expect_warning(expect_equal(
        compare_performance(m1, m2, metrics = "AICc", estimator = "REML")$AICc,
        c(177.52804, 182.88598),
        tolerance = 1e-3
      ))
      expect_equal(model_performance(m1, metrics = "AICc", estimator = "REML")$AICc, 177.52804, tolerance = 1e-3)
      expect_equal(performance_aicc(m1, estimator = "REML"), 177.52804, tolerance = 1e-3)

      # default - ML
      expect_equal(
        compare_performance(m1, m2, metrics = "AICc")$AICc,
        c(174.5701, 182.88598),
        tolerance = 1e-3
      )
      expect_equal(model_performance(m1, metrics = "AICc")$AICc, 174.5701, tolerance = 1e-3)
      expect_equal(performance_aicc(m1), 174.5701, tolerance = 1e-3)
    })
  }
}
