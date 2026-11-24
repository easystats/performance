test_that("r2_mlm_Rxy", {
  model <- lm(cbind(qsec, drat) ~ wt + mpg, data = mtcars)
  expect_equal(r2_mlm(model)[["Symmetric Rxy"]], 0.68330688076502, tolerance = 1e-3)
})

test_that("r2_mlm_Pxy", {
  model <- lm(cbind(qsec, drat) ~ wt + mpg, data = mtcars)
  expect_equal(r2_mlm(model)[["Asymmetric Pxy"]], 0.407215267524997, tolerance = 1e-3)
})

test_that("r2_mlm handles factor and interaction predictors via design matrix", {
  d <- mtcars
  d$cyl_f <- factor(d$cyl)

  # Factor predictor (previously errored in stats::cancor)
  m_fac <- lm(cbind(qsec, drat) ~ cyl_f + wt, data = d)
  out_fac <- r2_mlm(m_fac)
  wilks_fac <- 1 - det(stats::cov(stats::residuals(m_fac))) / det(stats::cov(d[, c("qsec", "drat")]))
  expect_equal(out_fac[["Symmetric Rxy"]], wilks_fac, tolerance = 1e-6)

  # Interaction predictor (previously ignored interaction term in Rxy)
  m_int <- lm(cbind(qsec, drat) ~ wt * mpg, data = d)
  out_int <- r2_mlm(m_int)
  wilks_int <- 1 - det(stats::cov(stats::residuals(m_int))) / det(stats::cov(d[, c("qsec", "drat")]))
  expect_equal(out_int[["Symmetric Rxy"]], wilks_int, tolerance = 1e-6)
})
