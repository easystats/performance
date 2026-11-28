test_that("r2_coxsnell", {
  model <- glm(vs ~ wt + mpg, data = mtcars, family = "binomial")
  expect_equal(
    r2_coxsnell(model),
    c(`Cox & Snell's R2` = 0.440140715155838),
    tolerance = 1e-3
  )
})

test_that("r2_coxsnell.glm uses log-likelihood for dispersion families", {
  m_gauss <- glm(mpg ~ wt, data = mtcars, family = gaussian())
  m_gauss_scaled <- glm(I(mpg / 100) ~ wt, data = mtcars, family = gaussian())
  ols_r2 <- summary(lm(mpg ~ wt, data = mtcars))$r.squared
  expect_equal(as.numeric(r2_coxsnell(m_gauss)), ols_r2, tolerance = 1e-6)
  expect_equal(as.numeric(r2_coxsnell(m_gauss_scaled)), ols_r2, tolerance = 1e-6)

  m_gamma <- glm(mpg ~ wt, data = mtcars, family = Gamma())
  m_gamma0 <- glm(mpg ~ 1, data = mtcars, family = Gamma())
  expected_gamma <- 1 -
    exp(-2 * (as.numeric(logLik(m_gamma)) - as.numeric(logLik(m_gamma0))) / nrow(mtcars))
  expect_equal(as.numeric(r2_coxsnell(m_gamma)), expected_gamma, tolerance = 1e-6)

  m_ig <- glm(mpg ~ wt, data = mtcars, family = inverse.gaussian())
  m_ig0 <- glm(mpg ~ 1, data = mtcars, family = inverse.gaussian())
  expected_ig <- 1 -
    exp(-2 * (as.numeric(logLik(m_ig)) - as.numeric(logLik(m_ig0))) / nrow(mtcars))
  expect_equal(as.numeric(r2_coxsnell(m_ig)), expected_ig, tolerance = 1e-6)
})

test_that("r2_coxsnell.glmmTMB uses log-likelihood for dispersion families", {
  skip_if_not_installed("glmmTMB")
  m <- glmmTMB::glmmTMB(mpg ~ wt, data = mtcars, family = gaussian())
  ols_r2 <- summary(lm(mpg ~ wt, data = mtcars))$r.squared
  expect_equal(as.numeric(r2_coxsnell(m)), ols_r2, tolerance = 1e-4)
})
