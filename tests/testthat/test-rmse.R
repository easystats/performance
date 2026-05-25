test_that("rmse", {
  skip_if_not_installed("lme4")
  data(sleepstudy, package = "lme4")

  m1.1 <- glm(Reaction ~ Days, data = sleepstudy, family = gaussian())
  m1.2 <- glm(Reaction ~ Days, data = sleepstudy, family = gaussian("log"))
  m1.3 <- glm(Reaction ~ Days, data = sleepstudy, family = gaussian("inverse"))

  m2.1 <- glm(Reaction ~ Days, data = sleepstudy, family = inverse.gaussian())
  m2.2 <- glm(Reaction ~ Days, data = sleepstudy, family = inverse.gaussian("log"))
  m2.3 <- glm(Reaction ~ Days, data = sleepstudy, family = inverse.gaussian("inverse"))

  expect_message(
    cp <- compare_performance(m1.1, m1.2, m1.3, m2.1, m2.2, m2.3),
    "seem to be identical"
  )
  expect_equal(
    cp$RMSE,
    c(47.4489, 47.39881, 47.38701, 47.41375, 47.39979, 47.38705),
    tolerance = 1e-3
  )
})

test_that("rmse, ci", {
  data(mtcars)
  model <- lm(mpg ~ hp + gear, data = mtcars)
  # analytical
  out <- performance_rmse(model, ci = 0.95, ci_method = "analytical")
  expect_equal(out$CI_low, 2.30486, tolerance = 1e-4)
  expect_equal(out$CI_high, 3.79093, tolerance = 1e-4)

  # bootstrapped
  set.seed(123)
  out <- performance_rmse(model, ci = 0.95, ci_method = "boot")
  expect_equal(out$CI_low, 1.9494, tolerance = 1e-3)
  expect_equal(out$CI_high, 3.38406, tolerance = 1e-3)

  # bootstrapped, mixed models
  skip_on_cran()
  skip_if_not_installed("lme4")
  data(sleepstudy, package = "lme4")
  m <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
  set.seed(123)
  out <- performance_rmse(m, ci = 0.95, iterations = 100)
  expect_equal(out$CI_low, 26.26066, tolerance = 1e-3)
  expect_equal(out$CI_high, 32.5642, tolerance = 1e-3)
})
