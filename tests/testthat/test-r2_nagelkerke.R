context("r2_nagelkerke")

test_that("r2_nagelkerke", {
  model <- glm(vs ~ wt + mpg, data = mtcars, family = "binomial")
  testthat::expect_equal(r2_nagelkerke(model), 0.5899593 , tolerance = 0.2)
})
