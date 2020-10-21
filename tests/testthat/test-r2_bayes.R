if (require("testthat") && require("performance")) {
  if (require("BayesFactor")) {
    test_that("r2_BayesFactor", {
      data(mtcars)
      set.seed(1)
      BFM <- generalTestBF(mpg ~ cyl * gear, data = mtcars, progress = FALSE)
      r_BF <- r2(BFM[4], ci = 0.89)
      r_CI <- attr(r_BF, "CI")$R2_Bayes

      testthat::expect_equal(r_BF$R2_Bayes, 0.71, tol = 0.05)
      testthat::expect_equal(r_CI$CI_low, 0.62, tol = 0.05)
      testthat::expect_equal(r_CI$CI_high, 0.79, tol = 0.05)

      r_post <- r2_posterior(BFM[1])
      testthat::expect_is(r_post, "data.frame")

      r_BF <- r2(BFM, average = TRUE)
      testthat::expect_equal(r_BF$R2_Bayes, 0.72, tol = 0.05)

      # with random effects:
      mtcars$gear <- factor(mtcars$gear)
      model <- lmBF(mpg ~ hp + cyl + gear + gear:wt, mtcars, progress = FALSE, whichRandom = c("gear", "gear:wt"))
      r_BF <- r2(model, ci = 0.89)
      r_CI <- attr(r_BF, "CI")$R2_Bayes

      testthat::expect_equal(r_BF$R2_Bayes, 0.36, tol = 0.05)
      testthat::expect_equal(r_BF$R2_Bayes_marginal, 0.21, tol = 0.05)
      testthat::expect_equal(r_CI$CI_low, 0.27, tol = 0.05)
      testthat::expect_equal(r_CI$CI_high, 0.54, tol = 0.05)
    })
  }
}
