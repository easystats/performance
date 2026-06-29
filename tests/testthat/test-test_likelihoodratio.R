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

skip_on_cran()
skip_if_not_installed("lme4")

m1 <- suppressMessages(lme4::lmer(
  Sepal.Length ~ Petal.Width + (1 | Species),
  data = iris
))
m2 <- suppressMessages(lme4::lmer(
  Sepal.Length ~ Petal.Width + Petal.Length + (1 | Species),
  data = iris
))
m3 <- suppressMessages(lme4::lmer(
  Sepal.Length ~ Petal.Width * Petal.Length + (1 | Species),
  data = iris
))

test_that("test_likelihoodratio - lme4 ML", {
  t1 <- test_lrt(m1, m2, m3)
  t2 <- suppressMessages(anova(m1, m2, m3))
  expect_equal(attributes(t1)$estimator, "ml")
  expect_equal(t1$Chi2, c(NA, 85.26365, 0.84141), tolerance = 1e-3)
  expect_equal(t1$p, c(NA, 0, 0.35899), tolerance = 1e-3)
  # close, but not the same
  expect_equal(t1$p, t2$`Pr(>Chisq)`, tolerance = 1e-1)
  expect_equal(t1$Chi2, t2$Chisq, tolerance = 1e-1)
})

test_that("test_likelihoodratio - lme4 OLS", {
  t2 <- test_lrt(m1, m2, m3, estimator = "ols")
  expect_equal(attributes(t2)$estimator, "ols")
  expect_equal(t2$Chi2, c(NA, 105.73844, 1.04346), tolerance = 1e-3)
  expect_equal(t2$p, c(NA, 0, 0.30702), tolerance = 1e-3)
})

test_that("test_likelihoodratio - lme4 REML", {
  expect_warning(t3 <- test_lrt(m1, m2, m3, estimator = "REML"))
  expect_equal(attributes(t3)$estimator, "reml")
  expect_equal(t3$Chi2, c(NA, 89.32933, 2.85635), tolerance = 1e-3)
  expect_equal(t3$p, c(NA, 0, 0.09101), tolerance = 1e-3)
})

m1 <- glm(am ~ mpg, data = mtcars, family = binomial())
m2 <- glm(am ~ mpg + hp, data = mtcars, family = binomial())
m3 <- glm(am ~ mpg + hp + vs, data = mtcars, family = binomial())

test_that("test_likelihoodratio - glm", {
  t1 <- anova(m1, m2, m3, test = "LRT")
  t2 <- test_lrt(m1, m2, m3)
  expect_equal(t1$`Pr(>Chi)`, t2$p, tolerance = 1e-3)
  expect_equal(t1$Deviance, t2$Chi2, tolerance = 1e-3)
  expect_equal(attributes(t2)$estimator, "ml")
})

m1 <- lm(Sepal.Length ~ Petal.Width, data = iris)
m2 <- lm(Sepal.Length ~ Petal.Width + Species, data = iris)
m3 <- lm(Sepal.Length ~ Petal.Width * Species, data = iris)

test_that("test_likelihoodratio - lm", {
  t1 <- anova(m1, m2, m3, test = "LRT")
  t2 <- test_lrt(m1, m2, m3)
  expect_equal(t1$`Pr(>Chi)`, t2$p, tolerance = 1e-3)
  expect_equal(attributes(t2)$estimator, "ols")
})

test_that("test_likelihoodratio - print p-digits", {
  skip_if_not_installed("knitr")
  data(mtcars)
  m1 <- lm(mpg ~ hp, data = mtcars)
  m2 <- lm(mpg ~ hp * as.factor(gear), data = mtcars)

  expect_snapshot(test_likelihoodratio(m1, m2))
  expect_snapshot(print_md(test_likelihoodratio(m1, m2), p_digits = 3))
})

test_that("test_likelihoodratio - Criterion values (lm)", {
  m1 <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
  m2 <- lm(mpg ~ wt + cyl + gear, data = mtcars)

  # Check ML estimator since OLS defaults to .test_wald()
  rez <- test_likelihoodratio(m2, m1, estimator = "ML")

  # Check if column exists
  expect_true("Criterion" %in% colnames(rez))

  # Check exact values against manual -2LL calculation
  ll1 <- as.numeric(stats::logLik(m1))
  ll2 <- as.numeric(stats::logLik(m2))
  expect_equal(rez$Criterion, -2 * c(ll2, ll1), tolerance = 1e-3)

  # Check that the difference in Criterion matches Chi2 exactly
  expect_equal(rez$Chi2[2], abs(diff(rez$Criterion)), tolerance = 1e-3)
})

test_that("test_likelihoodratio - Criterion values (lme4)", {
  skip_if_not_installed("lme4")

  m1 <- suppressMessages(lme4::lmer(
    Sepal.Length ~ Petal.Width + (1 | Species),
    data = iris,
    REML = FALSE
  ))
  m2 <- suppressMessages(lme4::lmer(
    Sepal.Length ~ Petal.Width + Petal.Length + (1 | Species),
    data = iris,
    REML = FALSE
  ))

  rez <- test_likelihoodratio(m1, m2, estimator = "ML")

  expect_true("Criterion" %in% colnames(rez))

  # Check values
  ll1 <- as.numeric(stats::logLik(m1))
  ll2 <- as.numeric(stats::logLik(m2))
  expect_equal(rez$Criterion, -2 * c(ll1, ll2), tolerance = 1e-3)

  # Check math
  expect_equal(rez$Chi2[2], abs(diff(rez$Criterion)), tolerance = 1e-3)
})

test_that("test_likelihoodratio - Criterion values (lavaan)", {
  skip_if_not_installed("lavaan")

  structure1 <- " visual  =~ x1 + x2 + x3
                  textual =~ x4 + x5 + x6
                  speed   =~ x7 + x8 + x9
                  visual ~~ textual + speed "
  m1 <- suppressWarnings(lavaan::cfa(structure1, data = lavaan::HolzingerSwineford1939))

  structure2 <- " visual  =~ x1 + x2 + x3
                  textual =~ x4 + x5 + x6
                  speed   =~ x7 + x8 + x9
                  visual ~~ 0 * textual + speed "
  m2 <- suppressWarnings(lavaan::cfa(structure2, data = lavaan::HolzingerSwineford1939))

  rez <- test_likelihoodratio(m1, m2)

  expect_true("Criterion" %in% colnames(rez))

  ll1 <- as.numeric(lavaan::fitMeasures(m1, "logl"))
  ll2 <- as.numeric(lavaan::fitMeasures(m2, "logl"))
  expect_equal(rez$Criterion, -2 * c(ll1, ll2), tolerance = 1e-3)
  expect_equal(rez$Chi2[2], abs(rez$Criterion[2] - rez$Criterion[1]), tolerance = 1e-3)
})
