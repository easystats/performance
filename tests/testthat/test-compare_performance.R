if (require("testthat") && require("performance")) {
  data(iris)
  lm1 <- lm(Sepal.Length ~ Species, data = iris)
  lm2 <- lm(Sepal.Length ~ Species + Petal.Length, data = iris)
  lm3 <- lm(Sepal.Length ~ Species * Petal.Length, data = iris)
  lm4 <- lm(Sepal.Length ~ Species * Petal.Length, data = iris[-1, ])

  test_that("compare_performance", {
    expect_equal(
      colnames(compare_performance(lm1, lm2, lm3)),
      c("Model", "Type", "AIC", "BIC", "R2", "R2_adjusted", "RMSE", "Sigma", "BF", "p")
    )

    expect_warning(
      expect_equal(
        colnames(compare_performance(lm1, lm2, lm3, lm4)),
        c("Model", "Type", "AIC", "BIC", "R2", "R2_adjusted", "RMSE", "Sigma")
      )
    )
    expect_equal(
      colnames(compare_performance(lm1, lm2, lm3, lm4, verbose = FALSE)),
      c("Model", "Type", "AIC", "BIC", "R2", "R2_adjusted", "RMSE", "Sigma")
    )
  })
}
