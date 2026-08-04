test_that("mgcv::gam", {
  skip_if_not_installed("mgcv")
  model <- mgcv::gam(mpg ~ s(hp), data = mtcars)

  out <- model_performance(model)
  expect_equal(
    out,
    data.frame(
      AIC = 168.696946079216,
      AICc = 170.664160555419,
      BIC = 175.466446314585,
      R2 = 0.757192545893888,
      RMSE = 2.92303700585394,
      Sigma = 3.10378496459189
    ),
    tolerance = 1e-4,
    ignore_attr = TRUE
  )
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
#   r2(model$mer, tolerance = 1e+100)
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
