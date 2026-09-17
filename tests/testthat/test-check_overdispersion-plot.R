skip_if_not_installed("DHARMa")

test_that("simulated dispersion plots use the same prediction baseline", {
  model <- glm(c(3, 7) ~ 1, family = poisson)
  local_mocked_bindings(
    simulate_residuals = function(...) {
      list(
        fittedPredictedResponse = c(2, 4),
        observedResponse = c(3, 7),
        simulatedResponse = rbind(c(1, 3, 4), c(2, 8, 11))
      )
    }
  )

  out <- performance:::.model_diagnostic_overdispersion(
    model,
    residual_type = "simulated"
  )
  # The predictions differ from the simulation means. The reference must
  # include that difference, not just the variance around each simulation mean.
  expect_identical(out$Predicted, c(2, 4))
  expect_identical(out$Res2, c(1, 9))
  expect_identical(out$V, c(2, 23))
  expect_identical(out$StdRes, c(1 / sqrt(2), 3 / sqrt(23)))
})

test_that("mixed Poisson dispersion plots agree with simulations, issue 464", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("DHARMa", minimum_version = "0.5.0")

  set.seed(101)
  d <- data.frame(
    x = runif(1000),
    f = factor(sample.int(200, size = 1000, replace = TRUE))
  )
  d$y <- suppressMessages(
    lme4::simulate.formula(
      ~ x + (1 | f),
      family = poisson,
      newdata = d,
      newparams = list(theta = 1, beta = c(0, 2))
    )[[1]]
  )
  model <- lme4::glmer(y ~ x + (1 | f), data = d, family = poisson)

  for (setting in c("conditional", "unconditional")) {
    out <- check_overdispersion(model, simulateREs = setting)
    plot_data <- performance:::.model_diagnostic_overdispersion(
      model,
      simulateREs = setting
    )
    expect_true(attr(out, "simulated"))
    expect_gt(out$p_value, 0.05)
    expect_equal(out$dispersion_ratio, 1, tolerance = 0.15)
    expect_equal(sum(plot_data$Res2) / sum(plot_data$V), 1, tolerance = 0.15)
    if (setting == "conditional") {
      # E[(Y - a)^2] = Var(Y) + (E[Y] - a)^2, with Y conditional Poisson.
      mu <- fitted(model)
      expected <- mu + (mu - plot_data$Predicted)^2
      expect_equal(sum(plot_data$V) / sum(expected), 1, tolerance = 0.05)
    }
    # A Poisson variance evaluated at the fixed-effects prediction misses
    # the random-effects contribution to these squared residuals.
    expect_gt(sum(plot_data$Res2) / sum(plot_data$Predicted), 10)
  }

  skip_if_not_installed("see")
  checks <- check_model(model, check = "overdispersion", simulateREs = "unconditional")
  expect_identical(checks$OVERDISPERSION, plot_data)
  expect_identical(plot(out, simulateREs = "unconditional")$data$V, plot_data$V)
})

test_that("simulated dispersion plots still identify overdispersed models", {
  skip_if_not_installed("glmmTMB")
  data(Salamanders, package = "glmmTMB")
  model <- glmmTMB::glmmTMB(
    count ~ mined + spp + (1 | site),
    family = poisson,
    data = Salamanders
  )

  out <- performance:::.model_diagnostic_overdispersion(model)
  expect_gt(sum(out$Res2) / sum(out$V), 2)
  expect_gt(check_overdispersion(model)$dispersion_ratio, 2)
})

test_that("non-simulated Poisson dispersion plots retain the family variance", {
  d <- data.frame(y = c(1, 3, 2, 6), x = 1:4)
  model <- glm(y ~ x, data = d, family = poisson)
  out <- performance:::.model_diagnostic_overdispersion(model, residual_type = "normal")

  expect_identical(out$V, unname(fitted(model)))
  expect_equal(out$Res2, unname(residuals(model, type = "response")^2), tolerance = 1e-12)
  expect_identical(out$StdRes, unname(residuals(model, type = "pearson")))
})
