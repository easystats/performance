skip_if_not_installed("metafor")
skip_if_not_installed("metadat")
data(dat.bcg, package = "metadat")
dat <- metafor::escalc(
  measure = "RR", ai = tpos, bi = tneg, ci = cpos,
  di = cneg, data = dat.bcg
)

test_that("model_performance.rma", {
  model <- metafor::rma(yi, vi, data = dat, method = "REML")
  mp <- model_performance(model, estimator = "REML")
  expect_null(mp$R2)
  expect_equal(mp$AIC, 28.40474, tolerance = 1e-3)
  expect_equal(mp$I2, 0.9222139, tolerance = 1e-3)
  expect_identical(colnames(mp), c(
    "AIC", "BIC", "I2", "H2", "TAU2", "CochransQ",
    "p_CochransQ", "df_error", "Omnibus", "p_Omnibus"
  ))
})

test_that("check_outliers.rma", {
  model <- metafor::rma(yi, vi, data = dat, slab = author, method = "REML")
  out <- check_outliers(model)
  expect_equal(
    out,
    c(
      FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE,
      FALSE, FALSE, FALSE, FALSE, FALSE
    ),
    ignore_attr = TRUE
  )
  expect_identical(attributes(out)$outlier_var, c("Hart & Sutherland", "TPT Madras"))
  expect_snapshot(out)
})

test_that("model_performance.rma", {
  model <- metafor::rma(yi, vi, mods = cbind(ablat, year), data = dat)
  mp <- model_performance(model, estimator = "REML")
  expect_equal(mp$R2, 0.6463217, tolerance = 1e-3)
  expect_equal(mp$AIC, 24.21375, tolerance = 1e-3)
  expect_equal(mp$I2, 0.719778, tolerance = 1e-3)
  expect_identical(colnames(mp), c(
    "AIC", "BIC", "I2", "H2", "TAU2", "CochransQ",
    "p_CochransQ", "df_error", "Omnibus", "p_Omnibus", "R2"
  ))
})
