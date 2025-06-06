test_that("performance_reliability - frequentist", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("glmmTMB")

  data(iris)
  data <- iris
  data$Place <- as.factor(rep(c("P1", "P2", "P3", "P4", "P5", "P6"), each = 25))

  m <- lme4::lmer(Sepal.Width ~ Petal.Width + (Petal.Width | Place), data = data)
  out <- performance_reliability(m)
  expect_identical(dim(out), c(2L, 3L))
  expect_equal(out$Reliability, c(0.87504, 0.42078), tolerance = 1e-3)

  m <- glmmTMB::glmmTMB(Sepal.Width ~ Petal.Width + (Petal.Width | Place), data = data)
  out <- performance_reliability(m)
  expect_identical(dim(out), c(2L, 3L))
  expect_equal(out$Reliability, c(0.84685, 0.32117), tolerance = 1e-3)
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
