test_that("test_wald - lm", {
  m1 <- lm(Sepal.Length ~ Petal.Width * Species, data = iris)
  m2 <- lm(Sepal.Length ~ Petal.Width + Species, data = iris)
  m3 <- lm(Sepal.Length ~ Petal.Width, data = iris)

  rez <- test_wald(m1, m2, m3)
  ref <- as.data.frame(anova(m1, m2, m3))

  expect_equal(rez$`F`, ref$`F`, tolerance = 1e-4)
  expect_equal(rez$p, ref$`Pr(>F)`, tolerance = 1e-4)

  # setting test = "LRT" is not expected, but should at least not fail
  expect_warning(rez <- test_wald(m1, m2, test = "LRT"))
  expect_equal(rez$Name, c("..1", "..2"))

  # Reversed
  m3 <- m1
  m1 <- lm(Sepal.Length ~ Petal.Width, data = iris)

  rez <- test_wald(m1, m2, m3)
  ref <- as.data.frame(anova(m1, m2, m3))

  expect_equal(rez$`F`, ref$`F`, tolerance = 1e-4)
  expect_equal(rez$p, ref$`Pr(>F)`, tolerance = 1e-4)
})

test_that("test_wald - glm", {
  m1 <- glm(vs ~ disp + hp + drat, data = mtcars, family = "binomial")
  m2 <- glm(vs ~ disp + hp, data = mtcars, family = "binomial")
  m3 <- glm(vs ~ disp, data = mtcars, family = "binomial")

  expect_warning(rez <- test_wald(m1, m2, m3))
  ref <- as.data.frame(anova(m1, m2, m3, test = "LRT"))

  expect_equal(rez$Chi2, abs(ref$Deviance), tolerance = 1e-4)
  expect_equal(rez$p, ref$`Pr(>Chi)`, tolerance = 1e-4)
})
