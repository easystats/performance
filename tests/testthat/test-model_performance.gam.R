if (requiet("gamm4") && requiet("mgcv")) {
  test_that("mgcv::gam", {
    model <- mgcv::gam(mpg ~ s(hp), data = mtcars)

    # TODO: improve output
    expect_equal(as.numeric(performance::r2(model)$R2), 0.7348, tolerance = 0.01)
  })


  # test_that("gamm4::gamm4 - no random", {
  #   model <- gamm4::gamm4(mpg ~ s(hp), data=mtcars)
  #
  #   performance::r2(model$mer)
  #   performance::r2(model$gam)
  # })
  #
  #
  # test_that("gamm4::gamm4 - random", {
  #   model <- gamm4::gamm4(mpg ~ s(hp), random = ~ (1|vs), data=mtcars)
  #
  #   performance::r2(model$mer)
  #   performance::r2(model$gam)
  # })
  #
  # test_that("mgcv::gamm", {
  #   model <- mgcv::gamm(mpg ~ s(hp), random = list(vs = ~1), data=mtcars)
  #
  #   performance::r2(model$lme)
  #   performance::r2(model$gam)
  # })
}
