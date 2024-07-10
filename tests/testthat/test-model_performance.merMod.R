test_that("model_performance.merMod", {
  skip_on_cran()
  skip_if_not_installed("curl")
  skip_if_offline()
  skip_if_not_installed("httr2")

  model <- insight::download_model("lmerMod_1")
  expect_equal(model_performance(model, estimator = "ML")$AIC, AIC(logLik(model, REML = FALSE)), tolerance = 0.01)
  expect_equal(model_performance(model, estimator = "REML")$AIC, AIC(model), tolerance = 0.01)

  model <- insight::download_model("merMod_1")
  expect_equal(model_performance(model)$AIC, AIC(model), tolerance = 0.01)
  expect_equal(model_performance(model, estimator = "REML")$AIC, AIC(model), tolerance = 0.01)
  expect_equal(model_performance(model)$AIC, 23.58593, tolerance = 0.01)

  model <- insight::download_model("merMod_2")
  expect_equal(model_performance(model)$AIC, 21.4729, tolerance = 0.01)
})


test_that("model_performance.merMod AICc", {
  skip_on_cran()
  skip_if_not_installed("lme4")
  m1 <- lme4::lmer(mpg ~ disp + am + (1 | cyl), data = mtcars)
  m2 <- lm(mpg ~ hp + vs, data = mtcars)

  # REML
  expect_message(expect_equal(
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
  # default model_performance is REML
  expect_equal(model_performance(m1, metrics = "AICc")$AICc, 177.52804, tolerance = 1e-3)
  expect_equal(model_performance(m1, metrics = "AICc", estimator = "ML")$AICc, 174.5701, tolerance = 1e-3)
  # default performance_aic is REML
  expect_equal(performance_aicc(m1), 177.52804, tolerance = 1e-3)
  expect_equal(performance_aicc(m1, estimator = "ML"), 174.5701, tolerance = 1e-3)
})
