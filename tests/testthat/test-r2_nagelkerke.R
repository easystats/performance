context("r2_nagelkerke")

test_that("r2_nagelkerke", {
  model <- glm(vs ~ wt + mpg, data = mtcars, family = "binomial")
  testthat::expect_equal(r2_nagelkerke(model), c(`Nagelkerke's R2` = 0.589959301837163), tolerance = 1e-3)
})
