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


test_that("compare_performance, LOOIC weights when not all models have LOOIC", {
  skip_on_cran()
  skip_if_not_installed("brms")
  skip_if_not_installed("loo")
  skip_if_not_installed("curl")
  skip_if_offline()
  skip_if_not_installed("httr2")

  m_brms <- insight::download_model("brms_1")
  skip_if(is.null(m_brms))
  m_brms2 <- m_brms
  m_lm <- lm(insight::find_formula(m_brms)$conditional, data = insight::get_data(m_brms))

  # only one model has LOOIC, so there are no stacking weights (#738)
  out <- compare_performance(m_lm, m_brms, verbose = FALSE)
  expect_identical(out$Name, c("m_lm", "m_brms"))
  expect_true(is.na(out$LOOIC[1]))
  expect_false(is.na(out$LOOIC[2]))
  expect_identical(out$LOOIC_wt, c(NA_real_, NA_real_))
  # a missing weight prints as an empty cell, not as a weight of 1
  formatted <- format(out)
  expect_identical(formatted[["LOOIC (weights)"]][1], "")
  expect_identical(formatted[["AIC (weights)"]][2], "")
  expect_identical(formatted[["AIC (weights)"]][1], "156.0")

  # stacking weights for the models that have LOOIC, NA for the others
  out <- compare_performance(m_lm, m_brms, m_brms2, verbose = FALSE)
  expect_identical(out$Name, c("m_lm", "m_brms", "m_brms2"))
  expect_true(is.na(out$LOOIC_wt[1]))
  expect_equal(sum(out$LOOIC_wt[2:3]), 1, tolerance = 1e-6)

  out <- compare_performance(m_lm, m_brms, m_brms2, rank = TRUE, verbose = FALSE)
  expect_identical(nrow(out), 3L)
})


test_that("compare_performance, no LOOIC weights for models fit to different numbers of observations", {
  skip_on_cran()
  skip_if_not_installed("brms")
  skip_if_not_installed("loo")
  skip_if_not_installed("curl")
  skip_if_offline()
  skip_if_not_installed("httr2")

  m_brms <- insight::download_model("brms_1")
  m_brms4 <- insight::download_model("brms_4")
  skip_if(is.null(m_brms) || is.null(m_brms4))
  m_brms2 <- m_brms

  # collect all warnings, so that the test sees how many there are
  collect_warnings <- function(expr) {
    warns <- character()
    value <- withCallingHandlers(
      suppressMessages(expr),
      warning = function(w) {
        warns <<- c(warns, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    list(value = value, warnings = warns)
  }

  # brms_1 has 32 observations, brms_4 has 150
  res <- collect_warnings(compare_performance(m_brms, m_brms4))
  expect_identical(res$value$LOOIC_wt, c(NA_real_, NA_real_))
  expect_equal(res$value$LOOIC, c(157.1964, 188.0575), tolerance = 1e-4, ignore_attr = TRUE)
  expect_length(res$warnings, 1)
  expect_match(res$warnings, "different numbers of observations", fixed = TRUE)
  expect_match(res$warnings, "32", fixed = TRUE)
  expect_match(res$warnings, "150", fixed = TRUE)
  expect_false(any(grepl("not a multiple of vector length", res$warnings, fixed = TRUE)))

  res <- collect_warnings(compare_performance(m_brms, m_brms4, rank = TRUE))
  expect_identical(nrow(res$value), 2L)

  # no weights for a subset of the models with LOOIC
  res <- collect_warnings(compare_performance(m_brms, m_brms2, m_brms4))
  expect_identical(res$value$LOOIC_wt, rep(NA_real_, 3))
  expect_length(res$warnings, 1)
  expect_match(res$warnings, "different numbers of observations", fixed = TRUE)

  # no warning with verbose = FALSE
  expect_silent({
    out <- suppressMessages(compare_performance(m_brms, m_brms4, verbose = FALSE))
  })
  expect_identical(out$LOOIC_wt, c(NA_real_, NA_real_))

  # models with the same number of observations keep their weights
  out <- suppressMessages(compare_performance(m_brms, m_brms2, verbose = FALSE))
  expect_equal(out$LOOIC_wt, c(0.5, 0.5), tolerance = 1e-6)
})


test_that("compare_performance, no LOOIC weights when one row count is a multiple of the other", {
  skip_if_not_installed("loo")

  m1 <- lm(mpg ~ wt, data = mtcars)
  m2 <- lm(mpg ~ wt, data = rbind(mtcars, mtcars))
  m3 <- lm(mpg ~ wt, data = mtcars[1:20, ])

  # LOO values with one row per observation, except for the model with 20
  # observations, which has no LOO values (like a glm next to a brms model)
  local_mocked_bindings(
    model_performance = function(model, ...) {
      n <- insight::n_obs(model)
      out <- data.frame(R2 = 0.5)
      if (n != 20) {
        out$LOOIC <- 100 + n
        attr(out, "loo") <- list(
          pointwise = matrix(-seq_len(n) / n, ncol = 1, dimnames = list(NULL, "elpd_loo"))
        )
      }
      out
    }
  )

  expect_warning(
    out <- suppressMessages(compare_performance(m1, m2)),
    "different numbers of observations"
  )
  expect_identical(out$LOOIC_wt, c(NA_real_, NA_real_))
  expect_warning(
    suppressMessages(compare_performance(m1, m2)),
    "32 and 64"
  )

  expect_warning(
    out <- suppressMessages(compare_performance(m1, m2, m3)),
    "different numbers of observations"
  )
  expect_identical(out$LOOIC_wt, rep(NA_real_, 3))
})


test_that("compare_performance, RMSE and Sigma still rank in the right direction", {
  data(mtcars)
  l_best <- lm(mpg ~ wt + cyl + hp, data = mtcars)
  l_worst <- lm(mpg ~ 1, data = mtcars)

  out <- compare_performance(l_best, l_worst, metrics = c("RMSE", "SIGMA"), rank = TRUE)

  expect_identical(out$Name, c("l_best", "l_worst"))
  expect_true(all(diff(out$RMSE) > 0))
  expect_true(all(diff(out$Sigma) > 0))
  expect_true(all(diff(out$Performance_Score) < 0))
})
