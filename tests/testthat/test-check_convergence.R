if (requiet("lme4")) {
  data(cbpp)
  data(sleepstudy)
  set.seed(1)
  cbpp$x <- rnorm(nrow(cbpp))
  cbpp$x2 <- runif(nrow(cbpp))

  model <- suppressWarnings(glmer(
    cbind(incidence, size - incidence) ~ period + x + x2 + (1 + x | herd),
    data = cbpp,
    family = binomial()
  ))

  test_that("check_convergence", {
    expect_true(check_convergence(model))
    expect_equal(check_convergence(model), structure(TRUE, gradient = 0.000280307452338331), tolerance = 1e-3)
  })

  model <- lmer(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)

  test_that("check_convergence", {
    expect_true(check_convergence(model))
  })
}
