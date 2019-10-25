if (require("testthat") && require("performance")) {
  context("check_convergence")

  data(iris)
  iris$`a m` <- iris$Species
  iris$`Sepal Width` <- iris$Sepal.Width
  m1 <- lm(`Sepal Width` ~ Petal.Length + `a m` * log(Sepal.Length), data = iris)
  m2 <- lm(Sepal.Width ~ Petal.Length + Species * log(Sepal.Length), data = iris)

}