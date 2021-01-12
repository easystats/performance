if (require("testthat") &&
  require("lavaan") &&
  require("lmtest")) {

  test_that("test_performance - nested", {

    # Decreasing
    m1 <- lm(Sepal.Length ~ Petal.Width * Species, data = iris)
    m2 <- lm(Sepal.Length ~ Petal.Width + Species, data = iris)
    m3 <- lm(Sepal.Length ~ Petal.Width, data = iris)

    rez <- test_performance(m1, m2, m3)

    expect_equal(rez$Name, c("m1", "m2", "m3"), ignore_attr = TRUE)

    # Increasing
    # TODO: Increasing order must be fixed and double checked, because the empty line should be the bottom one (?)
    # m1 <- lm(Sepal.Length ~ Petal.Width, data = iris)
    # m2 <- lm(Sepal.Length ~ Petal.Width + Species, data = iris)
    # m3 <- lm(Sepal.Length ~ Petal.Width * Species, data = iris)
    #
    # rez <- test_performance(m1, m2, m3)
  })
}
