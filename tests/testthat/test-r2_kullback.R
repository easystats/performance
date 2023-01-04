test_that("r2_kullback", {
  model <- glm(vs ~ wt + mpg, data = mtcars, family = "binomial")
  expect_equal(r2_kullback(model), c(`Kullback-Leibler R2` = 0.3834), tolerance = 1e-3)
  expect_equal(r2_kullback(model, adjust = FALSE), c(`Kullback-Leibler R2` = 0.4232), tolerance = 1e-3)
})
