test_that("model_performance.merMod", {
  skip_on_cran()
  skip_if_not_installed("curl")
  skip_if_offline()
  skip_if_not_installed("httr2")

  model <- insight::download_model("lmerMod_1")
  skip_if(is.null(model))
  expect_equal(
    model_performance(model, estimator = "ML")$AIC,
    AIC(logLik(model, REML = FALSE)),
    tolerance = 0.01
  )
  expect_equal(
    model_performance(model, estimator = "REML")$AIC,
    AIC(model),
    tolerance = 0.01
  )

  model <- insight::download_model("merMod_1")
  skip_if(is.null(model))
  expect_equal(model_performance(model)$AIC, AIC(model), tolerance = 0.01)
  expect_equal(
    model_performance(model, estimator = "REML")$AIC,
    AIC(model),
    tolerance = 0.01
  )
  expect_equal(model_performance(model)$AIC, 23.58593, tolerance = 0.01)

  model <- insight::download_model("merMod_2")
  skip_if(is.null(model))
  expect_equal(model_performance(model)$AIC, 21.4729, tolerance = 0.01)
})


test_that("model_performance.merMod fits the null model once for R2 and ICC", {
  skip_on_cran()
  skip_if_not_installed("glmmTMB")
  data(Salamanders, package = "glmmTMB")
  m <- glmmTMB::glmmTMB(
    count ~ mined + (1 | site),
    family = poisson,
    data = Salamanders
  )

  # reference values from R2 and ICC computed on their own, before the mock,
  # so that their null-model fits are not counted
  r2_ref <- r2(m)
  icc_ref <- icc(m)

  # count the null-model fits
  n_fits <- 0
  null_model_orig <- insight::null_model
  local_mocked_bindings(
    null_model = function(model, ...) {
      n_fits <<- n_fits + 1
      null_model_orig(model, ...)
    },
    .package = "insight"
  )

  out <- model_performance(m, metrics = c("R2", "ICC"))
  expect_identical(n_fits, 1)
  # same results as R2 and ICC computed on their own
  expect_equal(out$R2_conditional, r2_ref$R2_conditional, tolerance = 1e-8, ignore_attr = TRUE)
  expect_equal(out$R2_marginal, r2_ref$R2_marginal, tolerance = 1e-8, ignore_attr = TRUE)
  expect_equal(out$ICC, icc_ref$ICC_adjusted, tolerance = 1e-8)

  # a null model supplied by the user is used for both, and not refitted
  nm <- null_model_orig(m)
  n_fits <- 0
  out_nm <- model_performance(m, metrics = c("R2", "ICC"), null_model = nm)
  expect_identical(n_fits, 0)
  expect_equal(out_nm, out, tolerance = 1e-8, ignore_attr = TRUE)

  # only one of the two indices: nothing to share, fitted once as before
  n_fits <- 0
  model_performance(m, metrics = "R2")
  expect_identical(n_fits, 1)
})


test_that("model_performance.merMod AICc", {
  skip_on_cran()
  skip_if_not_installed("lme4")
  m1 <- lme4::lmer(mpg ~ disp + am + (1 | cyl), data = mtcars)
  m2 <- lm(mpg ~ hp + vs, data = mtcars)

  # REML
  expect_message(expect_equal(
    compare_performance(m1, m2, metrics = "AICc", estimator = "REML")$AICc,
    c(177.52804, 182.88598),
    tolerance = 1e-3
  ))
  expect_equal(
    model_performance(m1, metrics = "AICc", estimator = "REML")$AICc,
    177.52804,
    tolerance = 1e-3
  )
  expect_equal(performance_aicc(m1, estimator = "REML"), 177.52804, tolerance = 1e-3)

  # default - ML
  expect_equal(
    compare_performance(m1, m2, metrics = "AICc")$AICc,
    c(174.5701, 182.88598),
    tolerance = 1e-3
  )
  # default model_performance is REML
  expect_equal(model_performance(m1, metrics = "AICc")$AICc, 177.52804, tolerance = 1e-3)
  expect_equal(
    model_performance(m1, metrics = "AICc", estimator = "ML")$AICc,
    174.5701,
    tolerance = 1e-3
  )
  # default performance_aic is REML
  expect_equal(performance_aicc(m1), 177.52804, tolerance = 1e-3)
  expect_equal(performance_aicc(m1, estimator = "ML"), 174.5701, tolerance = 1e-3)
})
