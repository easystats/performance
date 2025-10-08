test_that("test_performance - nested", {
  skip_if_not_installed("lavaan")
  skip_if_not_installed("lmtest")

  # Decreasing
  m1 <- lm(Sepal.Length ~ Petal.Width * Species, data = iris)
  m2 <- lm(Sepal.Length ~ Petal.Width + Species, data = iris)
  m3 <- lm(Sepal.Length ~ Petal.Width, data = iris)

  rez <- test_performance(m1, m2, m3)
  expect_equal(rez$Name, c("m1", "m2", "m3"), ignore_attr = TRUE)

  rez <- test_performance(list(m1, m2, m3))
  expect_equal(rez$Name, c("Model 1", "Model 2", "Model 3"), ignore_attr = TRUE)

  models <- list(m1, m2, m3)
  rez <- test_performance(models)
  expect_equal(rez$Name, c("Model 1", "Model 2", "Model 3"), ignore_attr = TRUE)

  models <- list(Interaction = m1, NoInteraction = m2, SingleTerm = m3)
  rez <- test_performance(models)
  expect_equal(
    rez$Name,
    c("Interaction", "NoInteraction", "SingleTerm"),
    ignore_attr = TRUE
  )

  # Increasing
  # TODO: Increasing order must be fixed and double checked, because the empty line should be the bottom one (?)
  # m1 <- lm(Sepal.Length ~ Petal.Width, data = iris)
  # m2 <- lm(Sepal.Length ~ Petal.Width + Species, data = iris)
  # m3 <- lm(Sepal.Length ~ Petal.Width * Species, data = iris)
  #
  # rez <- test_performance(m1, m2, m3)
})

test_that("test_performance - single model", {
  m1 <- lm(Sepal.Length ~ 1, data = iris)
  m2 <- lm(Sepal.Length ~ Petal.Width, data = iris)
  expect_message(test_performance(m2), regex = "Only one model")
  expect_silent(test_performance(m2, verbose = FALSE))

  out1 <- test_performance(m2, verbose = FALSE)
  out2 <- test_performance(m1, m2, verbose = FALSE)
  expect_equal(out1$Omega2, out2$Omega2, tolerance = 1e-3)
})
