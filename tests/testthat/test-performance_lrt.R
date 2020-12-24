if (require("testthat") &&
  require("performance") &&
  require("lavaan")) {

  test_that("performance_lrt - regression models", {
    m1 <- lm(mpg ~ wt + cyl, data = mtcars)
    m2 <- lm(mpg ~ wt + cyl + gear, data = mtcars)
    m3 <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)

    # anova
    rez <- performance_lrt(m1, m2, m3, estimator = "OLS")

    ref <- anova(m1, m2, m3, test = "LRT")
    testthat::expect_equal(ref$`Pr(>Chi)`, rez$p, tolerance = 1e-03)

    # lrtest
    rez <- performance_lrt(m1, m2, m3, estimator = "ML")

    # ref <- lmtest::lrtest(m1, m2, m3)
    # ref$`Pr(>Chisq)`
    testthat::expect_equal(c(NA, 0.4747344, 0.5302030), rez$p, tolerance = 1e-03)
    # ref$LogLik
    testthat::expect_equal(c(-74.00503, -73.74957, -73.55256), rez$LogLik, tolerance = 1e-03)
    # ref$Chisq
    testthat::expect_equal(c(NA, 0.5109349, 0.3940024), rez$Chi2, tolerance = 1e-03)
  })
}
