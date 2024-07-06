test_that("r2_ferarri", {
  skip_if_not_installed("betareg")
  data("GasolineYield", package = "betareg")
  model <- betareg::betareg(yield ~ batch + temp, data = GasolineYield)
  out <- r2_ferrari(model)
  expect_equal(out$R2, summary(model)$pseudo.r.squared, tolerance = 1e-3, ignore_attr = TRUE)
})
