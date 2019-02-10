context("performance_R2_tjur")

test_that("performance_R2_tjur", {
  model <- glm(vs ~ wt + mpg, data = mtcars, family = "binomial")
  testthat::expect_equal(performance_R2_tjur(model), 0.477, tolerance = 0.2)
})

# test_that("model_parameters.brmsfit", {
#   testthat::skip_on_travis()
#   library(brms)
#
#   model <- brms::brm(mpg ~ wt + cyl, data = mtcars)
#   # params <- model_parameters(model, standardize=TRUE, estimate=c("median", "mean", "MAP"), test=c("pd", "rope", "p_map"))
#   # testthat::expect_equal(nrow(params), 3)
#   # testthat::expect_equal(ncol(params), 19)
# })
