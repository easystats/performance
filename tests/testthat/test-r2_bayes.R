skip_on_os("linux")

test_that("r2_BayesFactor", {
  skip_if_not_installed("BayesFactor")
  set.seed(1)
  BFM <- BayesFactor::generalTestBF(mpg ~ cyl * gear, data = mtcars, progress = FALSE)
  r_BF <- r2(BFM[4], ci = 0.89)
  r_CI <- attr(r_BF, "CI")$R2_Bayes

  expect_equal(r_BF$R2_Bayes, 0.71, tolerance = 0.05)
  expect_equal(r_CI$CI_low, 0.62, tolerance = 0.05)
  expect_equal(r_CI$CI_high, 0.79, tolerance = 0.05)

  r_post <- r2_posterior(BFM[1])
  expect_s3_class(r_post, "data.frame")

  r_BF <- r2(BFM, average = TRUE)
  expect_equal(r_BF$R2_Bayes, 0.72, tolerance = 0.05)

  # with random effects:
  skip_if_not_installed("BayesFactor", minimum_version = "0.9.12.4.3")
  mtcars$gear <- factor(mtcars$gear)
  model <- BayesFactor::lmBF(mpg ~ hp + cyl + gear + gear:wt, mtcars, progress = FALSE, whichRandom = c("gear", "gear:wt"))
  r_BF <- r2(model, ci = 0.89)
  r_CI <- attr(r_BF, "CI")$R2_Bayes

  expect_equal(r_BF$R2_Bayes, 0.36, tolerance = 0.05)
  expect_equal(r_BF$R2_Bayes_marginal, 0.21, tolerance = 0.05)
  expect_equal(r_CI$CI_low, 0.27, tolerance = 0.05)
  expect_equal(r_CI$CI_high, 0.54, tolerance = 0.05)
})
