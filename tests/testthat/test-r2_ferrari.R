test_that("r2_ferarri", {
  skip_if_not_installed("betareg")
  data("GasolineYield", package = "betareg")
  model <- betareg::betareg(yield ~ batch + temp, data = GasolineYield)
  out <- r2_ferrari(model)
  expect_equal(out$R2, summary(model)$pseudo.r.squared, tolerance = 1e-3, ignore_attr = TRUE)
})


test_that("r2_ferarri", {
  skip_if_not_installed("betareg")
  skip_if_not_installed("glmmTMB")
  data("GasolineYield", package = "betareg")
  model <- glmmTMB::glmmTMB(
    yield ~ batch + temp,
    data = GasolineYield,
    family = glmmTMB::beta_family()
  )
  out <- r2_ferrari(model)
  expect_equal(out$R2, c(`Ferrari's R2` = 0.96173), tolerance = 1e-3, ignore_attr = TRUE)
})


test_that("r2_ferarri", {
  skip_if_not_installed("betareg")
  skip_if_not_installed("glmmTMB")
  skip_if_not_installed("lme4")
  data(sleepstudy, package = "lme4")
  sleepstudy$y <- datawizard::normalize(sleepstudy$Reaction)
  m <- glmmTMB::glmmTMB(
    y ~ Days,
    data = sleepstudy,
    family = glmmTMB::ordbeta()
  )
  out <- r2(m)
  expect_equal(out$R2, c(`Ferrari's R2` = 0.2354701), tolerance = 1e-3, ignore_attr = TRUE)
})
