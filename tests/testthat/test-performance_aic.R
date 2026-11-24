test_that("performance_aic works", {
  skip_if_not_installed("parameters")
  data(fish, package = "parameters")

  model_norm <- lm(count ~ factor(persons), fish)
  model_pois <- glm(count ~ factor(persons), fish, family = poisson())
  expect_equal(performance_aic(model_norm), 1930.755, tolerance = 1e-2)
  expect_equal(performance_aic(model_pois), 2758.236, tolerance = 1e-2)
})

test_that("performance_aic for log-model works", {
  skip_if_not_installed("parameters")
  data(fish, package = "parameters")
  model_lnorm <- lm(log1p(count) ~ factor(persons), fish)
  expect_equal(performance_aic(model_lnorm), 1025.352, tolerance = 1e-2)
})

test_that("performance_aic Jacobian", {
  m1 <- lm(disp ~ hp, data = mtcars)
  m2 <- lm(sqrt(disp) ~ hp, data = mtcars)
  expect_equal(performance_aic(m1), 372.8247, tolerance = 1e-2)
  expect_equal(performance_aic(m2), 367.1239, tolerance = 1e-2)
})

test_that("performance_aic lme4 default", {
  skip_if_not_installed("lme4")
  m1 <- lme4::lmer(Sepal.Length ~ Petal.Length + (1 | Species), data = iris)
  expect_equal(performance_aic(m1), AIC(m1), tolerance = 1e-2)
  expect_equal(performance_aic(m1, estimator = "ML"), 125.0043, tolerance = 1e-2)
  m2 <- lme4::lmer(Sepal.Length ~ Petal.Length + (1 | Species), data = iris, REML = FALSE)
  expect_equal(performance_aic(m2, estimator = "REML"), 128.0054, tolerance = 1e-2)
  expect_message(performance_aic(m2), regex = "was not fitted")
})

test_that("performance_aicc works for Arima models", {
  m <- stats::arima(lh, order = c(1, 0, 0))
  ll <- stats::logLik(m)
  k <- attr(ll, "df")
  n <- stats::nobs(m)
  expected_aicc <- -2 * as.numeric(ll) + 2 * k * (n / (n - k - 1))

  expect_equal(performance_aicc(m), expected_aicc, tolerance = 1e-6)
  expect_gt(performance_aicc(m), stats::AIC(m))

  m_diff <- stats::arima(lh, order = c(1, 1, 0))
  ll_diff <- stats::logLik(m_diff)
  k_diff <- attr(ll_diff, "df")
  n_diff <- stats::nobs(m_diff)
  expected_aicc_diff <- -2 * as.numeric(ll_diff) + 2 * k_diff * (n_diff / (n_diff - k_diff - 1))

  expect_equal(performance_aicc(m_diff), expected_aicc_diff, tolerance = 1e-6)
})
