if (requiet("testthat") && requiet("performance")) {
  data("mtcars")
  mtcars$mpg <- floor(mtcars$mpg)

  model_lnorm <- lm(log(mpg) ~ factor(cyl), mtcars)
  model_norm <- lm(mpg ~ factor(cyl), mtcars)
  model_pois <- glm(mpg ~ factor(cyl), mtcars, family = poisson())

  test_that("performance_aic works", {
    expect_equal(performance_aic(model_norm), 170.8753, tolerance = 1e-2)
    expect_equal(performance_aic(model_pois), 173.591, tolerance = 1e-2)
  })

  test_that("performance_aic for log-model works", {
    expect_equal(performance_aic(model_lnorm), 168.3652, tolerance = 1e-2)
  })

  m1 <- lm(disp ~ hp, data = mtcars)
  m2 <- lm(sqrt(disp) ~ hp, data = mtcars)

  test_that("performance_aic Jacobian", {
    expect_equal(performance_aic(m1), 372.8247, tolerance = 1e-2)
    expect_equal(performance_aic(m2), 367.12394, tolerance = 1e-2)
  })
}
