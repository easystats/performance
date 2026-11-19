test_that("binomial expected variance uses the scale of the diagnostic", {
  set.seed(1)
  n <- sample(5:40, 100, replace = TRUE)
  x <- rnorm(100)
  k <- rbinom(100, n, plogis(0.5 + 0.3 * x))
  m <- glm(cbind(k, n - k) ~ x, family = binomial)
  p <- unname(fitted(m))
  info <- insight::model_info(m)

  expect_equal(
    .expected_variance(m, info, data.frame(Predicted = n * p), trials = n),
    n * p * (1 - p)
  )
  expect_equal(
    .expected_variance(m, info, data.frame(Predicted = p)),
    p * (1 - p)
  )
  expect_equal(
    .expected_variance(m, info, data.frame(Predicted = p), trials = 1),
    p * (1 - p)
  )
})


test_that("simulated binomial overdispersion uses counts and per-row trials", {
  set.seed(1)
  dat <- data.frame(
    n = sample(5:40, 100, replace = TRUE),
    x = rnorm(100)
  )
  dat$k <- rbinom(100, dat$n, plogis(0.5 + 0.3 * dat$x))
  dat$proportion <- dat$k / dat$n
  dat$x[c(3, 10)] <- NA

  models <- list(
    glm(cbind(k, n - k) ~ x, family = binomial, data = dat, na.action = na.exclude),
    glm(proportion ~ x, weights = n, family = binomial, data = dat, na.action = na.exclude)
  )
  keep <- !is.na(dat$x)
  local_mocked_bindings(
    .use_simulated_residuals = function(...) TRUE,
    .binomial_fitted = function(model) .fitted_rows(stats::fitted(model), model)
  )
  for (m in models) {
    p <- unname(.fitted_rows(fitted(m), m))
    n <- dat$n[keep]
    k <- dat$k[keep]

    # Emulate both DHARMa fitted-value scales without changing the observation scale.
    for (predicted in list(p, n * p)) {
      local({
        sim <- list(fittedPredictedResponse = predicted, observedResponse = k)
        local_mocked_bindings(simulate_residuals = function(...) sim)
        out <- .model_diagnostic_overdispersion(m, residual_type = "simulated")
        expect_equal(out$Predicted, n * p, ignore_attr = TRUE)
        expect_equal(out$Residuals, k - n * p, ignore_attr = TRUE)
        expect_equal(out$V, n * p * (1 - p), ignore_attr = TRUE)
        expect_equal(out$StdRes, (k - n * p) / sqrt(n * p * (1 - p)), ignore_attr = TRUE)
      })
    }
  }
})


test_that("grouped binomial diagnostics work with DHARMa", {
  skip_if_not_installed("DHARMa")
  set.seed(1)
  n <- sample(5:40, 100, replace = TRUE)
  x <- rnorm(100)
  k <- rbinom(100, n, plogis(0.5 + 0.3 * x))
  m <- glm(cbind(k, n - k) ~ x, family = binomial)
  p <- unname(.fitted_rows(DHARMa::getFitted(m), m))
  out <- .model_diagnostic_overdispersion(m, residual_type = "simulated")
  expect_equal(out$Residuals, k - n * p, ignore_attr = TRUE)
  expect_equal(out$V, n * p * (1 - p), ignore_attr = TRUE)
})


test_that("simulated Bernoulli overdispersion stays on the binary scale", {
  set.seed(2)
  dat <- data.frame(x = rnorm(100))
  dat$y <- rbinom(100, 1, plogis(dat$x / 2))
  m <- glm(y ~ x, family = binomial, data = dat)
  p <- unname(fitted(m))
  local_mocked_bindings(
    .use_simulated_residuals = function(...) TRUE,
    .binomial_fitted = function(model) stats::fitted(model),
    simulate_residuals = function(...) list(
      fittedPredictedResponse = p,
      observedResponse = dat$y
    )
  )
  out <- .model_diagnostic_overdispersion(m, residual_type = "simulated")
  expect_equal(out$Predicted, p, ignore_attr = TRUE)
  expect_equal(out$Residuals, dat$y - p, ignore_attr = TRUE)
  expect_equal(out$V, p * (1 - p), ignore_attr = TRUE)
})
