test_that("mgcv::gam", {
  skip_if_not_installed("mgcv")
  model <- mgcv::gam(mpg ~ s(hp), data = mtcars)

  # TODO: improve output
  expect_equal(as.numeric(performance::r2(model)$R2), 0.7348, tolerance = 0.01)
})

# test_that("gamm4::gamm4 - no random", {
#   skip_if_not_installed("gamm4")
#   model <- gamm4::gamm4(mpg ~ s(hp), data=mtcars)
#
#   r2(model$mer)
#   r2(model$gam)
# })
#
# test_that("gamm4::gamm4 - random", {
#   skip_if_not_installed("gamm4")
#   model <- gamm4::gamm4(mpg ~ s(hp), random = ~ (1|vs), data=mtcars)
#
#   r2(model$mer)
#   r2(model$gam)
# })
#
# test_that("mgcv::gamm", {
#   skip_if_not_installed("mgcv")
#   model <- mgcv::gamm(mpg ~ s(hp), random = list(vs = ~1), data=mtcars)
#
#   r2(model$lme)
#   r2(model$gam)
# })

