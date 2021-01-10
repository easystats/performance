if (require("testthat") &&
  require("nonnest2") &&
  require("lavaan")) {

  test_that("test_performance_vuong - nested", {
    m1 <- lm(Sepal.Length ~ Petal.Width * Species, data = iris)
    m2 <- lm(Sepal.Length ~ Petal.Width + Species, data = iris)
    m3 <- lm(Sepal.Length ~ Petal.Width, data = iris)

    rez <- test_performance_vuong(m1, m2, m3)

    ref <- nonnest2::vuongtest(m1, m2, nested=TRUE)
    testthat::expect_equal(rez[2, "Omega2"], ref$omega)
    testthat::expect_equal(rez[2, "p_Omega2"], ref$p_omega)
    testthat::expect_equal(rez[2, "LR"], ref$LRTstat)
    testthat::expect_equal(rez[2, "p_LR"], ref$p_LRT$A)

    ref <- nonnest2::vuongtest(m2, m3, nested=TRUE)
    testthat::expect_equal(rez[3, "Omega2"], ref$omega)
    testthat::expect_equal(rez[3, "p_Omega2"], ref$p_omega)
    testthat::expect_equal(rez[3, "LR"], ref$LRTstat)
    testthat::expect_equal(rez[3, "p_LR"], ref$p_LRT$A)
  })

  test_that("test_performance_vuong - non-nested", {
    m1 <- lm(Sepal.Length ~ Petal.Width, data = iris)
    m2 <- lm(Sepal.Length ~ Petal.Length, data = iris)
    m3 <- lm(Sepal.Length ~ Species, data = iris)

    rez <- test_performance_vuong(m1, m2, m3)

    ref <- nonnest2::vuongtest(m1, m2)
    testthat::expect_equal(rez[2, "Omega2"], ref$omega)
    testthat::expect_equal(rez[2, "p_Omega2"], ref$p_omega)
    testthat::expect_equal(rez[2, "LR"], ref$LRTstat)
    testthat::expect_equal(rez[2, "p_LR"], ref$p_LRT$B)

    ref <- nonnest2::vuongtest(m1, m3)
    testthat::expect_equal(rez[3, "Omega2"], ref$omega)
    testthat::expect_equal(rez[3, "p_Omega2"], ref$p_omega)
    testthat::expect_equal(rez[3, "LR"], ref$LRTstat)
    testthat::expect_equal(rez[3, "p_LR"], ref$p_LRT$A)
  })
}
