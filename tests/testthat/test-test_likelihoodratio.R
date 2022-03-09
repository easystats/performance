if (requiet("testthat") &&
  requiet("performance") &&
  requiet("lavaan")) {
  test_that("test_likelihoodratio - regression models", {
    m1 <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
    m2 <- lm(mpg ~ wt + cyl + gear, data = mtcars)
    m3 <- lm(mpg ~ wt + cyl, data = mtcars)

    # stats::anova()
    rez <- test_likelihoodratio(m1, m2, m3, estimator = "OLS")
    ref <- anova(m1, m2, m3, test = "LRT")
    expect_equal(ref$`Pr(>Chi)`, rez$p, tolerance = 1e-03)

    rez <- test_likelihoodratio(m3, m2, m1, estimator = "OLS")
    ref <- anova(m3, m2, m1, test = "LRT")
    expect_equal(ref$`Pr(>Chi)`, rez$p, tolerance = 1e-03)

    # lmtest::lrtest()
    rez <- test_likelihoodratio(m1, m2, m3, estimator = "ML")
    p <- c(NA, 0.5302030, 0.4747344) # lmtest::lrtest(m1, m2, m3)$`Pr(>Chisq)`
    expect_equal(p, rez$p, tolerance = 1e-03)

    rez <- test_likelihoodratio(m3, m2, m1, estimator = "ML")
    p <- c(NA, 0.4747344, 0.5302030) # lmtest::lrtest(m3, m2, m1)$`Pr(>Chisq)`
    expect_equal(p, rez$p, tolerance = 1e-03)
  })

  test_that("test_likelihoodratio - model names", {
    m1 <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
    m2 <- lm(mpg ~ wt + cyl + gear, data = mtcars)
    m3 <- lm(mpg ~ wt + cyl, data = mtcars)

    rez <- test_likelihoodratio(m1, m2, m3)
    expect_equal(rez$Name, c("m1", "m2", "m3"))

    rez <- test_likelihoodratio(list(m1, m2, m3))
    expect_equal(rez$Name, c("Model 1", "Model 2", "Model 3"))

    models <- list(m1, m2, m3)
    rez <- test_likelihoodratio(models)
    expect_equal(rez$Name, c("Model 1", "Model 2", "Model 3"))
  })

  test_that("test_likelihoodratio - reversed order", {
    m1 <- lm(mpg ~ wt + cyl, data = mtcars)
    m2 <- lm(mpg ~ wt + cyl + gear, data = mtcars)
    m3 <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)

    rez <- test_likelihoodratio(m1, m2, m3, estimator = "OLS")
    ref <- anova(m1, m2, m3, test = "LRT")
    expect_equal(ref$`Pr(>Chi)`, rez$p, tolerance = 1e-03)

    rez <- test_likelihoodratio(m1, m2, m3, estimator = "ML")
    p <- c(NA, 0.4747344, 0.5302030) # lmtest::lrtest(m1, m2, m3)$`Pr(>Chisq)`
    expect_equal(p, rez$p, tolerance = 1e-03)
  })
}
