test_that("r2_mlm_Rxy", {
  model <- lm(cbind(qsec, drat) ~ wt + mpg, data = mtcars)
  expect_equal(r2_mlm(model)[["Symmetric Rxy"]], 0.68330688076502, tolerance = 1e-3)
})

test_that("r2_mlm_Pxy", {
  model <- lm(cbind(qsec, drat) ~ wt + mpg, data = mtcars)
  expect_equal(r2_mlm(model)[["Asymmetric Pxy"]], 0.407215267524997, tolerance = 1e-3)
})
