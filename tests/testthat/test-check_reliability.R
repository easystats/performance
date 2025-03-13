test_that("check_reliability - frequentist", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("glmmTMB")

  data <- iris
  data$Place <- as.factor(rep(c("P1", "P2", "P3", "P4", "P5", "P6"), each = 25))

  m <- lme4::lmer(Sepal.Width ~ Petal.Width + (Petal.Width | Place), data = data)
  out <- check_reliability(m)
  testthat::expect_true(all(dim(out) == c(2, 5)))

  m <- glmmTMB::glmmTMB(Sepal.Width ~ Petal.Width + (Petal.Width | Place), data = data)
  out <- check_reliability(m)
  testthat::expect_true(all(dim(out) == c(2, 5)))
})

test_that("check_reliability - Bayesian", {
  skip_if_offline()
  skip_if_not_installed("curl")
  skip_if_not_installed("httr2")
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("rstantools")
  skip_if_not_installed("brms")

  library(rstanarm)


  # m <- rstanarm::stan_lmer(
  #   Sepal.Width ~ Petal.Width + (Petal.Width | Group),
  #   data = d,
  #   refresh = 0
  # )
  # m <- brms::brm(
  #   Sepal.Width ~ Petal.Width + (Petal.Width | Group),
  #   data = d,
  #   refresh = 0,
  #   backend="cmdstanr"
  # )
  m <- insight::download_model("brms_mixed_10")
  out <- check_reliability(m)
  testthat::expect_true(all(dim(out) == c(2, 5)))

  m <- insight::download_model("brms_sigma_3")
  out <- check_reliability(m)
  testthat::expect_true(all(dim(out) == c(4, 6)))
})
