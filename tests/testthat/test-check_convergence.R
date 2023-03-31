test_that("check_convergence", {
  skip_if_not_installed("lme4")

  data(cbpp, package = "lme4")
  set.seed(1)
  cbpp$x <- rnorm(nrow(cbpp))
  cbpp$x2 <- runif(nrow(cbpp))

  model <- suppressWarnings(lme4::glmer(
    cbind(incidence, size - incidence) ~ period + x + x2 + (1 + x | herd),
    data = cbpp,
    family = binomial()
  ))
  expect_true(check_convergence(model))
  expect_equal(
    check_convergence(model),
    structure(TRUE, gradient = 0.000280307452338331),
    tolerance = 1e-3
  )
})

test_that("check_convergence", {
  skip_if_not_installed("lme4")

  data(sleepstudy, package = "lme4")
  model <- lme4::lmer(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)
  expect_true(check_convergence(model))
})
