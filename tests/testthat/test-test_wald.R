if (require("testthat") &&
  require("lavaan") &&
  require("lmtest")) {

  test_that("test_wald - lm", {
    m1 <- lm(Sepal.Length ~ Petal.Width * Species, data = iris)
    m2 <- lm(Sepal.Length ~ Petal.Width + Species, data = iris)
    m3 <- lm(Sepal.Length ~ Petal.Width, data = iris)

    rez <- test_wald(m1, m2, m3)
    ref <- as.data.frame(anova(m1, m2, m3))

    testthat::expect_equal(rez$`F`, ref$`F`)
    testthat::expect_equal(rez$p, ref$`Pr(>F)`)
  })

  test_that("test_wald - glm", {
    m1 <- glm(vs ~ disp + hp + drat, data = mtcars, family="binomial")
    m2 <- glm(vs ~ disp + hp, data = mtcars, family="binomial")
    m3 <- glm(vs ~ disp, data = mtcars, family="binomial")

    rez <- test_wald(m1, m2, m3)
    # Why different?
    # ref <- lmtest::waldtest(m1, m2, m3)
    #
    # testthat::expect_equal(rez$`F`, ref$`F`)
    # testthat::expect_equal(rez$p, ref$`Pr(>F)`)
  })
}
