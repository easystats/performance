skip_on_cran()
skip_if_not_installed("rstanarm")
skip_if_not_installed("rstantools")

test_that("model_performance.stanreg", {
  skip_if_not_installed("curl")
  skip_if_offline()
  skip_if_not_installed("httr2")
  set.seed(333)
  model <- insight::download_model("stanreg_lm_1")
  skip_if(is.null(model))
  perf <- model_performance(model)

  expect_equal(perf$R2, 0.7398733, tolerance = 1e-3)
  expect_equal(perf$R2_adjusted, 0.7162912, tolerance = 1e-3)
  expect_equal(perf$ELPD, -83.49838, tolerance = 1e-3)

  model <- insight::download_model("stanreg_lm_2")
  skip_if(is.null(model))
  perf <- model_performance(model)

  expect_equal(perf$R2, 0.8168386, tolerance = 1e-3)
  expect_equal(perf$R2_adjusted, 0.7979026, tolerance = 1e-3)
  expect_equal(perf$ELPD, -78.38735, tolerance = 1e-3)

  model <- insight::download_model("stanreg_lmerMod_1")
  skip_if(is.null(model))
  perf <- model_performance(model)

  expect_equal(perf$R2, 0.642, tolerance = 1e-3)
  expect_equal(perf$R2_adjusted, 0.6053454, tolerance = 1e-3)
  expect_equal(perf$ELPD, -31.55849, tolerance = 1e-3)
})


test_that("model_performance.brmsfit", {
  skip_if_not_installed("curl")
  skip_if_offline()
  skip_if_not_installed("httr2")
  set.seed(333)

  model <- insight::download_model("brms_1")
  skip_if(is.null(model))
  expect_message({
    perf <- model_performance(model)
  })
  expect_equal(perf$R2, 0.8262673, tolerance = 1e-2)
  expect_equal(perf$R2_adjusted, 0.792831, tolerance = 1e-2)
  expect_equal(perf$ELPD, -78.59823, tolerance = 1e-2)
  expect_identical(colnames(perf), c(
    "ELPD", "ELPD_SE", "LOOIC", "LOOIC_SE", "WAIC", "R2", "R2_adjusted",
    "RMSE", "Sigma"
  ))

  model <- insight::download_model("brms_mixed_4")
  skip_if(is.null(model))
  expect_message({
    perf <- model_performance(model)
  })
  expect_equal(perf$R2, 0.954538, tolerance = 1e-2)
  expect_equal(perf$R2_adjusted, 0.9526158, tolerance = 1e-2)
  expect_equal(perf$ELPD, -70.40493, tolerance = 1e-2)
  expect_named(perf, c(
    "ELPD", "ELPD_SE", "LOOIC", "LOOIC_SE", "WAIC", "R2", "R2_marginal",
    "R2_adjusted", "R2_adjusted_marginal", "ICC", "RMSE", "Sigma"
  ))

  model <- insight::download_model("brms_ordinal_1")
  skip_if(is.null(model))
  perf <- suppressWarnings(model_performance(model))
  expect_equal(perf$R2, 0.8760015, tolerance = 1e-3)
  expect_equal(perf$ELPD, -11.65433, tolerance = 1e-3)
})


test_that("model_performance.BFBayesFactor", {
  skip_if_not_installed("BayesFactor")
  mod <- BayesFactor::ttestBF(mtcars$wt, mu = 3)
  expect_warning({
    p <- model_performance(mod)
  })
  expect_null(p)

  mod <- BayesFactor::ttestBF(mtcars$wt, factor(mtcars$am))
  expect_warning({
    p <- model_performance(mod)
  })
  expect_null(p)

  mods <- BayesFactor::contingencyTableBF(matrix(1:4, 2), sampleType = "indepMulti", fixedMargin = "cols")
  expect_warning({
    p <- model_performance(mod)
  })
  expect_null(p)

  mod <- BayesFactor::correlationBF(mtcars$wt, mtcars$am)
  expect_warning({
    p <- model_performance(mod)
  })
  expect_null(p)

  mod <- BayesFactor::proportionBF(y = 15, N = 25, p = 0.5)
  expect_warning({
    p <- model_performance(mod)
  })
  expect_null(p)

  t <- c(-0.15, 2.39, 2.42, 2.43, -0.15, 2.39, 2.42, 2.43)
  N <- c(100, 150, 97, 99, 99, 97, 100, 150)
  mod <- BayesFactor::meta.ttestBF(t, N)
  expect_warning({
    p <- model_performance(mod)
  })
  expect_null(p)

  skip_on_os("linux")
  mod <- BayesFactor::regressionBF(mpg ~ cyl, mtcars, progress = FALSE)
  modF <- lm(mpg ~ cyl, mtcars)
  p <- model_performance(mod)
  expect_equal(p$R2, unname(r2(modF)[[1]]), tolerance = 0.05)
  expect_equal(p$Sigma, sigma(modF), tolerance = 0.05)
})
