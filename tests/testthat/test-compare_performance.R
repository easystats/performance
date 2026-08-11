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
    capture.output(print(compare_performance(lm1, lm2, lm3), table_width = 85)),
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


test_that("compare_performance, lavaan", {
  skip_on_cran()
  skip_if_not_installed("lavaan")

  data(HolzingerSwineford1939, package = "lavaan")
  structure <- " visual  =~ x1 + x2 + x3
                 textual =~ x4 + x5 + x6
                 speed   =~ x7 + x8 + x9 "
  model1 <- lavaan::cfa(structure, data = HolzingerSwineford1939)
  model2 <- lavaan::cfa(structure, data = HolzingerSwineford1939)

  out <- performance::compare_performance(model1, model2)
  expect_identical(
    capture.output(print(out, ci_digits = 2, table_width = Inf)),
    c(
      "# Comparison of Model Performance Indices",
      "",
      "Name   |  Model | Chi2(24) | p (Chi2) | Baseline(36) | p (Baseline) |   GFI |  AGFI |   NFI |  NNFI |   CFI | RMSEA |    RMSEA  CI | p (RMSEA) |   RMR |  SRMR |   RFI |  PNFI |   IFI |   RNI | Loglikelihood |  AIC (weights) |  BIC (weights) | BIC_adjusted",
      "---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------",
      "model1 | lavaan |   85.306 |   < .001 |      918.852 |       < .001 | 0.959 | 0.894 | 0.907 | 0.896 | 0.931 | 0.092 | [0.07, 0.11] |    < .001 | 0.082 | 0.065 | 0.861 | 0.605 | 0.931 | 0.931 |     -3737.745 | 7517.5 (0.500) | 7595.3 (0.500) |     7528.739",
      "model2 | lavaan |   85.306 |   < .001 |      918.852 |       < .001 | 0.959 | 0.894 | 0.907 | 0.896 | 0.931 | 0.092 | [0.07, 0.11] |    < .001 | 0.082 | 0.065 | 0.861 | 0.605 | 0.931 | 0.931 |     -3737.745 | 7517.5 (0.500) | 7595.3 (0.500) |     7528.739"
    )
  )
})


test_that("compare_performance, Log_loss ranks in the right direction", {
  data(mtcars)
  m_best <- glm(am ~ mpg + hp + wt, data = mtcars, family = binomial())
  m_mid <- glm(am ~ mpg, data = mtcars, family = binomial())
  m_worst <- glm(am ~ 1, data = mtcars, family = binomial())

  out <- compare_performance(m_best, m_mid, m_worst, metrics = "LOGLOSS", rank = TRUE)

  # log-loss is an error measure, so the smallest value is the best model and
  # must come first once the table is ranked (#917)
  expect_identical(out$Name, c("m_best", "m_mid", "m_worst"))
  expect_true(all(diff(out$Log_loss) > 0))
  expect_true(all(diff(out$Performance_Score) < 0))
})


test_that("compare_performance, RMSE and Sigma still rank in the right direction", {
  data(mtcars)
  l_best <- lm(mpg ~ wt + cyl + hp, data = mtcars)
  l_worst <- lm(mpg ~ 1, data = mtcars)

  out <- compare_performance(l_best, l_worst, metrics = c("RMSE", "SIGMA"), rank = TRUE)

  expect_identical(out$Name, c("l_best", "l_worst"))
  expect_true(all(diff(out$RMSE) > 0))
  expect_true(all(diff(out$Performance_Score) < 0))
})
