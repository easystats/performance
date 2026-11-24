test_that("performance_reliability - frequentist", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("glmmTMB")

  data(iris)
  data <- iris
  data$Place <- as.factor(rep(c("P1", "P2", "P3", "P4", "P5", "P6"), each = 25))

  m <- lme4::lmer(Sepal.Width ~ Petal.Width + (Petal.Width | Place), data = data)
  out <- performance_reliability(m)
  expect_identical(dim(out), c(2L, 3L))
  expect_equal(out$Reliability, c(0.99610, 0.96359), tolerance = 1e-3)

  m <- glmmTMB::glmmTMB(Sepal.Width ~ Petal.Width + (Petal.Width | Place), data = data)
  out <- performance_reliability(m)
  expect_identical(dim(out), c(2L, 3L))
  v <- insight::get_variance(m)
  L <- min(table(data$Place))
  expected_glmmtmb <- c(
    v$var.intercept[["Place"]] / (v$var.intercept[["Place"]] + v$var.residual / L),
    v$var.slope[["Place.Petal.Width"]] / (v$var.slope[["Place.Petal.Width"]] + v$var.residual / L)
  )
  expect_equal(out$Reliability, expected_glmmtmb, tolerance = 1e-3)
})


test_that("performance_reliability matches Rouder & Mehrvarz formula and is scale-invariant", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("parameters")

  data(iris)
  d <- iris
  d$Place <- as.factor(rep(c("P1", "P2", "P3", "P4", "P5", "P6"), each = 25))

  m1 <- lme4::lmer(Sepal.Width ~ Petal.Width + (Petal.Width | Place), data = d)
  out1 <- performance_reliability(m1)

  v <- insight::get_variance(m1)
  L <- min(table(d$Place))
  gamma2 <- c(v$var.intercept[["Place"]], v$var.slope[["Place.Petal.Width"]]) / v$var.residual
  expected <- gamma2 / (gamma2 + 1 / L)

  expect_equal(out1$Reliability, expected, tolerance = 1e-6)
  expect_equal(out1$Reliability, c(0.99610, 0.96359), tolerance = 1e-4)

  # Reliability must be invariant to rescaling the response
  d$Sepal.Width_scaled <- d$Sepal.Width * 10
  m2 <- lme4::lmer(Sepal.Width_scaled ~ Petal.Width + (Petal.Width | Place), data = d)
  out2 <- performance_reliability(m2)
  expect_equal(out2$Reliability, out1$Reliability, tolerance = 1e-4)
})


test_that("performance_reliability - Bayesian", {
  skip_if_not_installed("curl")
  skip_if_offline()
  skip_if_not_installed("httr2")
  skip_if_not_installed("brms")

  m <- suppressWarnings(insight::download_model("brms_mixed_10"))
  skip_if(is.null(m))
  out <- performance_reliability(m)
  expect_identical(dim(out), c(2L, 3L))
  expect_equal(out$Reliability, c(0.97116, 0.80915), tolerance = 1e-3)
})

## FIXME: doesn't work yet for Bayesian models

# test_that("performance_reliability - Bayesian", {
#   skip_if_not_installed("curl")
#   skip_if_offline()
#   skip_if_not_installed("httr2")
#   skip_if_not_installed("brms")

#   # m <- rstanarm::stan_lmer(
#   #   Sepal.Width ~ Petal.Width + (Petal.Width | Group),
#   #   data = d,
#   #   refresh = 0
#   # )
#   # m <- brms::brm(
#   #   Sepal.Width ~ Petal.Width + (Petal.Width | Group),
#   #   data = d,
#   #   refresh = 0,
#   #   backend="cmdstanr"
#   # )
#   m <- insight::download_model("brms_mixed_10")
#   skip_if(is.null(m))
#   out <- performance_reliability(m)
#   expect_identical(dim(out), c(2L, 5L))

#   m <- insight::download_model("brms_sigma_3")
#   skip_if(is.null(m))
#   out <- performance_reliability(m)
#   expect_identical(dim(out), c(4L, 6L))
# })
