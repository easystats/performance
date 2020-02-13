if (require("testthat") && require("performance") && require("metafor")) {
  dat <- escalc(measure = "RR", ai = tpos, bi = tneg, ci = cpos, di = cneg, data = dat.bcg)

  test_that("model_performance.rma", {
    model <- rma(yi, vi, data = dat, method = "REML")
    mp <- model_performance(model)
    expect_null(mp$R2)
    expect_equal(mp$AIC, 28.40474, tolerance = 1e-3)
    expect_equal(mp$I2, 0.9222139, tolerance = 1e-3)
    expect_equal(colnames(mp), c("AIC", "BIC", "I2", "H2", "TAU2"))
  })

  test_that("model_performance.rma", {
    model <- rma(yi, vi, mods = cbind(ablat, year), data = dat)
    mp <- model_performance(model)
    expect_equal(mp$R2, 0.6463217, tolerance = 1e-3)
    expect_equal(mp$AIC, 24.21375, tolerance = 1e-3)
    expect_equal(mp$I2, 0.719778, tolerance = 1e-3)
    expect_equal(colnames(mp), c("AIC", "BIC", "I2", "H2", "TAU2", "R2"))
  })
}
