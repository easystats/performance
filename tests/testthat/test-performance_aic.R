if (requiet("parameters")) {
  data("fish")

  model_lnorm <- lm(log1p(count) ~ factor(persons), fish)
  model_norm <- lm(count ~ factor(persons), fish)
  model_pois <- glm(count ~ factor(persons), fish, family = poisson())

  test_that("performance_aic works", {
    expect_equal(performance_aic(model_norm), 1930.755, tolerance = 1e-2)
    expect_equal(performance_aic(model_pois), 2758.236, tolerance = 1e-2)
  })

  test_that("performance_aic for log-model works", {
    expect_equal(performance_aic(model_lnorm), 1025.352, tolerance = 1e-2)
  })

  data(mtcars)
  m1 <- lm(disp ~ hp, data = mtcars)
  m2 <- lm(sqrt(disp) ~ hp, data = mtcars)

  test_that("performance_aic Jacobian", {
    expect_equal(performance_aic(m1), 372.8247, tolerance = 1e-2)
    expect_equal(performance_aic(m2), 367.1239, tolerance = 1e-2)
  })

  if (requiet("lme4")) {
    data(iris)
    m1 <- lmer(Sepal.Length ~ Petal.Length + (1 | Species), data = iris)

    test_that("performance_aic lme4 default", {
      expect_equal(performance_aic(m1), AIC(m1), tolerance = 1e-2)
      expect_equal(performance_aic(m1, estimator = "ML"), 125.0043, tolerance = 1e-2)
    })
  }
}
