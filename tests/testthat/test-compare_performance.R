if (requiet("testthat") && requiet("performance")) {
  data(iris)
  lm1 <- lm(Sepal.Length ~ Species, data = iris)
  lm2 <- lm(Sepal.Length ~ Species + Petal.Length, data = iris)
  lm3 <- lm(Sepal.Length ~ Species * Petal.Length, data = iris)
  lm4 <- lm(Sepal.Length ~ Species * Petal.Length, data = iris[-1, ])

  test_that("compare_performance", {
    expect_equal(
      colnames(compare_performance(lm1, lm2, lm3)),
      c("Name", "Model", "AIC", "AIC_wt", "BIC", "BIC_wt", "R2", "R2_adjusted", "RMSE", "Sigma")
    )

    expect_warning(
      expect_equal(
        colnames(compare_performance(lm1, lm2, lm3, lm4)),
        c("Name", "Model", "AIC", "AIC_wt", "BIC", "BIC_wt", "R2", "R2_adjusted", "RMSE", "Sigma")
      )
    )
    expect_equal(
      colnames(compare_performance(lm1, lm2, lm3, lm4, verbose = FALSE)),
      c("Name", "Model", "AIC", "AIC_wt", "BIC", "BIC_wt", "R2", "R2_adjusted", "RMSE", "Sigma")
    )

    expect_silent(compare_performance(lm1, lm2, estimator = "REML"))
  })

  if (requiet("lme4")) {
    m1 <- lmer(Petal.Length ~ Sepal.Length + (1 | Species), data = iris)
    m2 <- lmer(Petal.Length ~ Sepal.Length + Sepal.Width + (1 | Species), data = iris)

    test_that("compare_performance, REML fit", {
      expect_silent(compare_performance(m1, m2))
      expect_warning(compare_performance(m1, m2, estimator = "REML"))
    })
  }
}
