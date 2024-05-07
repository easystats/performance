test_that("check_distribution", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("randomForest")
  data(sleepstudy, package = "lme4")
  model <<- lme4::lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
  out <- check_distribution(model)

  expect_identical(
    out$Distribution,
    c(
      "bernoulli", "beta", "beta-binomial", "binomial", "cauchy",
      "chi", "exponential", "F", "gamma", "half-cauchy", "inverse-gamma",
      "lognormal", "neg. binomial (zero-infl.)", "negative binomial",
      "normal", "pareto", "poisson", "poisson (zero-infl.)", "tweedie",
      "uniform", "weibull"
    )
  )
  expect_equal(
    out$p_Residuals,
    c(
      0, 0, 0, 0, 0.90625, 0, 0, 0, 0.0625, 0, 0, 0.03125, 0, 0,
      0, 0, 0, 0, 0, 0, 0
    ),
    tolerance = 1e-4
  )
  expect_equal(
    out$p_Response,
    c(
      0, 0, 0, 0, 0, 0, 0, 0, 0.34375, 0, 0, 0.65625, 0, 0, 0, 0,
      0, 0, 0, 0, 0
    ),
    tolerance = 1e-4
  )

  expect_snapshot(print(out))
})
