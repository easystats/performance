if (require("testthat") && require("performance") && require("insight")
    && require("censReg") && require("AER") && require("ordinal")) {
  context("model_performance different models")

  data("Affairs", package = "AER")

  m1 <- insight::download_model("betareg_1")
  m2 <- insight::download_model("censReg_1")
  m3 <- insight::download_model("clm_1")
  m4 <- insight::download_model("clm2_1")
  m5 <- insight::download_model("ivreg_1")

  test_that("model_performance various", {
    expect_equal(model_performance(m1)$R2, 0.9617312, tolerance = 1e-4)
    expect_equal(model_performance(m2)$R2, 0.1333737, tolerance = 1e-4)
    expect_equal(model_performance(m3)$R2, 0.4042792, tolerance = 1e-4)
    expect_equal(model_performance(m4)$R2, 0.4042792, tolerance = 1e-4)
    expect_equal(model_performance(m5)$R2, 0.4294224, tolerance = 1e-4)
  })
}