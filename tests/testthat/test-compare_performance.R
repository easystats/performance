test_that("compare_performance", {
  data(iris)
  lm1 <- lm(Sepal.Length ~ Species, data = iris)
  lm2 <- lm(Sepal.Length ~ Species + Petal.Length, data = iris)
  lm3 <- lm(Sepal.Length ~ Species * Petal.Length, data = iris)
  lm4 <- lm(Sepal.Length ~ Species * Petal.Length, data = iris[-1, ])

  expect_silent(expect_identical(
    colnames(compare_performance(lm1, lm2, lm3)),
    c(
      "Name",
      "Model",
      "AIC",
      "AIC_wt",
      "AICc",
      "AICc_wt",
      "BIC",
      "BIC_wt",
      "R2",
      "R2_adjusted",
      "RMSE",
      "Sigma"
    )
  ))

  expect_message(
    expect_identical(
      colnames(compare_performance(lm1, lm2, lm3, lm4)),
      c(
        "Name",
        "Model",
        "AIC",
        "AIC_wt",
        "AICc",
        "AICc_wt",
        "BIC",
        "BIC_wt",
        "R2",
        "R2_adjusted",
        "RMSE",
        "Sigma"
      )
    )
  )

  # table split
  expect_snapshot(print(compare_performance(lm1, lm2, lm3)))
  expect_snapshot(print(compare_performance(lm1, lm2, lm3), table_width = Inf))
  # vertical layout
  expect_snapshot(print(compare_performance(lm1, lm2, lm3), layout = "vertical"))
  expect_snapshot(print(
    compare_performance(lm1, lm2, lm3, lm4),
    layout = "vertical",
    table_width = 50
  ))

  expect_silent(expect_identical(
    colnames(compare_performance(lm1, lm2, lm3, lm4, verbose = FALSE)),
    c(
      "Name",
      "Model",
      "AIC",
      "AIC_wt",
      "AICc",
      "AICc_wt",
      "BIC",
      "BIC_wt",
      "R2",
      "R2_adjusted",
      "RMSE",
      "Sigma"
    )
  ))

  out <- compare_performance(lm1, lm2, lm3, lm4, verbose = FALSE)
  expect_identical(out$Name, c("lm1", "lm2", "lm3", "lm4"))

  models <- list(Interaction = lm3, NoInteraction = lm2, SingleTerm = lm1)
  rez <- compare_performance(models)
  expect_equal(
    rez$Name,
    c("Interaction", "NoInteraction", "SingleTerm"),
    ignore_attr = TRUE
  )

  out <- compare_performance(list(lm1, lm2, lm3, lm4), verbose = FALSE)
  expect_identical(
    colnames(out),
    c(
      "Name",
      "Model",
      "AIC",
      "AIC_wt",
      "AICc",
      "AICc_wt",
      "BIC",
      "BIC_wt",
      "R2",
      "R2_adjusted",
      "RMSE",
      "Sigma"
    )
  )
  expect_identical(out$Name, c("Model 1", "Model 2", "Model 3", "Model 4"))

  models <- list(lm1, lm2, lm3, lm4)
  out <- compare_performance(models, verbose = FALSE)
  expect_identical(
    colnames(out),
    c(
      "Name",
      "Model",
      "AIC",
      "AIC_wt",
      "AICc",
      "AICc_wt",
      "BIC",
      "BIC_wt",
      "R2",
      "R2_adjusted",
      "RMSE",
      "Sigma"
    )
  )
  expect_identical(out$Name, c("Model 1", "Model 2", "Model 3", "Model 4"))

  expect_silent(compare_performance(lm1, lm2, estimator = "REML"))
})

test_that("compare_performance, REML fit", {
  skip_if_not_installed("lme4")
  m1 <- lme4::lmer(Petal.Length ~ Sepal.Length + (1 | Species), data = iris)
  m2 <- lme4::lmer(Petal.Length ~ Sepal.Length + Sepal.Width + (1 | Species), data = iris)
  expect_silent(compare_performance(m1, m2))
  expect_message(compare_performance(m1, m2, estimator = "REML"))
})
