if (require("testthat") && require("performance") && require("lme4")) {
  .runThisTest <- Sys.getenv("RunAllperformanceTests") == "yes"

  if (.runThisTest) {
    test_that("model_performance.merMod", {
      library(insight)

      model <- insight::download_model("lmerMod_1")
      expect_equal(model_performance(model)$AIC, 71.59892, tolerance = 0.01)

      model <- insight::download_model("merMod_1")
      expect_equal(model_performance(model)$AIC, 23.58593, tolerance = .01)

      model <- insight::download_model("merMod_2")
      expect_equal(model_performance(model)$AIC, 21.4729, tolerance = .01)
    })
  }
}
