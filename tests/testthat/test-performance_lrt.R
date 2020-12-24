if (require("testthat") &&
  require("performance") &&
  require("lavaan")) {

  test_that("performance_lrt - regression models", {
    m1 <- lm(mpg ~ wt + cyl, data = mtcars)
    m2 <- lm(mpg ~ wt + cyl + gear, data = mtcars)
    m3 <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)

    # anova
    rez <- performance_lrt(m1, m2, m3, estimator="OLS")

    ref <- anova(m1, m2, test="LRT")
    testthat::expect_equal(ref$`Pr(>Chi)`[2], rez$p[2], tol=1e-06)

    ref <- anova(m1, m3, test="LRT")
    testthat::expect_equal(ref$`Pr(>Chi)`[2], rez$p[3], tol=1e-06)

    # lrtest
    rez <- performance_lrt(m1, m2, m3, estimator="ML")

    # lmtest::lrtest(m1, m2)$`Pr(>Chisq)`[2]
    testthat::expect_equal(0.4747344, rez$p[2], tol=1e-06)
    # lmtest::lrtest(m1, m2)$LogLik[2]
    testthat::expect_equal(-73.74957, rez$LogLik[2], tol=1e-06)

    # lmtest::lrtest(m1, m3)$`Pr(>Chisq)`[2]
    testthat::expect_equal(0.636056, rez$p[3], tol=1e-06)
  })

}
