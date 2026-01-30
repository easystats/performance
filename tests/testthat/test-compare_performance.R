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

  skip_on_cran()

  # table split
  expect_identical(
    capture.output(print(compare_performance(lm1, lm2, lm3))),
    c(
      "# Comparison of Model Performance Indices",
      "",
      "Name | Model | AIC (weights) | AICc (weights) | BIC (weights) |    R2 | R2 (adj.)",
      "---------------------------------------------------------------------------------",
      "lm1  |    lm | 231.5 (<.001) |  231.7 (<.001) | 243.5 (<.001) | 0.619 |     0.614",
      "lm2  |    lm | 106.2 (0.566) |  106.6 (0.611) | 121.3 (0.964) | 0.837 |     0.833",
      "lm3  |    lm | 106.8 (0.434) |  107.6 (0.389) | 127.8 (0.036) | 0.840 |     0.835",
      "",
      "Name |  RMSE | Sigma",
      "--------------------",
      "lm1  | 0.510 | 0.515",
      "lm2  | 0.333 | 0.338",
      "lm3  | 0.330 | 0.336"
    )
  )
  expect_identical(
    capture.output(print(compare_performance(lm1, lm2, lm3), table_width = Inf)),
    c(
      "# Comparison of Model Performance Indices",
      "",
      "Name | Model | AIC (weights) | AICc (weights) | BIC (weights) |    R2 | R2 (adj.) |  RMSE | Sigma",
      "-------------------------------------------------------------------------------------------------",
      "lm1  |    lm | 231.5 (<.001) |  231.7 (<.001) | 243.5 (<.001) | 0.619 |     0.614 | 0.510 | 0.515",
      "lm2  |    lm | 106.2 (0.566) |  106.6 (0.611) | 121.3 (0.964) | 0.837 |     0.833 | 0.333 | 0.338",
      "lm3  |    lm | 106.8 (0.434) |  107.6 (0.389) | 127.8 (0.036) | 0.840 |     0.835 | 0.330 | 0.336"
    )
  )
  # vertical layout
  expect_identical(
    capture.output(print(compare_performance(lm1, lm2, lm3), layout = "vertical")),
    c(
      "# Comparison of Model Performance Indices",
      "",
      "Metric         |           lm1 |           lm2 |           lm3",
      "--------------------------------------------------------------",
      "Model          |            lm |            lm |            lm",
      "AIC (weights)  | 231.5 (<.001) | 106.2 (0.566) | 106.8 (0.434)",
      "AICc (weights) | 231.7 (<.001) | 106.6 (0.611) | 107.6 (0.389)",
      "BIC (weights)  | 243.5 (<.001) | 121.3 (0.964) | 127.8 (0.036)",
      "R2             |         0.619 |         0.837 |         0.840",
      "R2 (adj.)      |         0.614 |         0.833 |         0.835",
      "RMSE           |         0.510 |         0.333 |         0.330",
      "Sigma          |         0.515 |         0.338 |         0.336"
    )
  )
  expect_identical(
    capture.output(print(
      compare_performance(lm1, lm2, lm3, lm4),
      layout = "vertical",
      table_width = 50
    )),
    c(
      "# Comparison of Model Performance Indices",
      "",
      "Metric         |           lm1 |           lm2",
      "----------------------------------------------",
      "Model          |            lm |            lm",
      "AIC (weights)  | 231.5 (<.001) | 106.2 (0.408)",
      "AICc (weights) | 231.7 (<.001) | 106.6 (0.454)",
      "BIC (weights)  | 243.5 (<.001) | 121.3 (0.933)",
      "R2             |         0.619 |         0.837",
      "R2 (adj.)      |         0.614 |         0.833",
      "RMSE           |         0.510 |         0.333",
      "Sigma          |         0.515 |         0.338",
      "",
      "Metric         |           lm3 |           lm4",
      "----------------------------------------------",
      "Model          |            lm |            lm",
      "AIC (weights)  | 106.8 (0.313) | 107.0 (0.279)",
      "AICc (weights) | 107.6 (0.289) | 107.8 (0.257)",
      "BIC (weights)  | 127.8 (0.035) | 128.0 (0.032)",
      "R2             |         0.840 |         0.840",
      "R2 (adj.)      |         0.835 |         0.834",
      "RMSE           |         0.330 |         0.331",
      "Sigma          |         0.336 |         0.337"
    )
  )

  expect_silent(expect_named(
    compare_performance(lm1, lm2, lm3, lm4, verbose = FALSE),
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
  expect_named(
    out,
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
  expect_named(
    out,
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
