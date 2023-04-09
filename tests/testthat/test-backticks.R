test_that("check_collinearity, backticks", {
  data(iris)
  iris$`a m` <- iris$Species
  iris$`Sepal Width` <- iris$Sepal.Width
  m1 <- lm(`Sepal Width` ~ Petal.Length + `a m` * log(Sepal.Length), data = iris)
  m2 <- lm(Sepal.Width ~ Petal.Length + Species * log(Sepal.Length), data = iris)

  expect_message(check_collinearity(m1))
  expect_identical(
    check_collinearity(m1, verbose = FALSE)$Term,
    c("Petal.Length", "a m", "log(Sepal.Length)", "a m:log(Sepal.Length)")
  )
  expect_identical(
    check_collinearity(m2, verbose = FALSE)$Term,
    c("Petal.Length", "Species", "log(Sepal.Length)", "Species:log(Sepal.Length)")
  )
})
