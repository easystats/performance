if (require("testthat") &&
  require("performance") &&
  require("lavaan")) {

  test_that("performance_lrt - regression models", {
    m1 <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
    m2 <- lm(mpg ~ wt + cyl + gear, data = mtcars)
    m3 <- lm(mpg ~ wt + cyl, data = mtcars)

    # stats::anova()
    rez <- performance_lrt(m1, m2, m3, estimator = "OLS")
    ref <- anova(m1, m2, m3, test = "LRT")
    expect_equal(ref$`Pr(>Chi)`, rez$p, tolerance = 1e-03)

    rez <- performance_lrt(m3, m2, m1, estimator = "OLS")
    ref <- anova(m3, m2, m1, test = "LRT")
    expect_equal(ref$`Pr(>Chi)`, rez$p, tolerance = 1e-03)

    # lmtest::lrtest()
    rez <- performance_lrt(m1, m2, m3, estimator = "ML")
    p <- c(NA, 0.5302030, 0.4747344)  # lmtest::lrtest(m1, m2, m3)$`Pr(>Chisq)`
    expect_equal(p, rez$p, tolerance = 1e-03)

    rez <- performance_lrt(m3, m2, m1, estimator = "ML")
    p <- c(NA, 0.4747344, 0.5302030)  # lmtest::lrtest(m3, m2, m1)$`Pr(>Chisq)`
    expect_equal(p, rez$p, tolerance = 1e-03)
  })
}
