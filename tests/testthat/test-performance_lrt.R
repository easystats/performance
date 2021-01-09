if (require("testthat") &&
  require("performance") &&
  require("lavaan")) {

  test_that("performance_lrt - regression models", {
    m1 <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
    m2 <- lm(mpg ~ wt + cyl + gear, data = mtcars)
    m3 <- lm(mpg ~ wt + cyl, data = mtcars)

    # anova
    rez <- performance_lrt(m1, m2, m3, estimator = "OLS")
    ref <- anova(m1, m2, m3, test = "LRT")
    expect_equal(ref$`Pr(>Chi)`, rez$p, tolerance = 1e-03)

    # lrtest
    rez <- performance_lrt(m1, m2, m3, estimator = "ML")
    p <- c(NA, 0.5302030, 0.4747344)  # lmtest::lrtest(m1, m2, m3)$`Pr(>Chisq)`
    expect_equal(p, rez$p, tolerance = 1e-03)
  })
}
