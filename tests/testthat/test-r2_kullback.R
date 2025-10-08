test_that("r2_kullback", {
  model <- glm(vs ~ wt + mpg, data = mtcars, family = "binomial")
  expect_equal(r2_kullback(model), c(`Kullback-Leibler R2` = 0.3834), tolerance = 1e-3)
  expect_equal(
    r2_kullback(model, adjust = FALSE),
    c(`Kullback-Leibler R2` = 0.4232),
    tolerance = 1e-3
  )
})

test_that("r2_kullback errors for non-supported", {
  skip_if_not_installed("pscl")
  data("bioChemists", package = "pscl")
  model <- pscl::zeroinfl(art ~ . | 1, data = bioChemists, dist = "negbin")
  expect_error(r2_kullback(model), regex = "This function only works")
})
