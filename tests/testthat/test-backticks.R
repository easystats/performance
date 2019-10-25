if (require("testthat") && require("performance")) {
  context("check_convergence")

  data(iris)
  iris$`a m` <- iris$Species
  iris$`Sepal Width` <- iris$Sepal.Width
  m <- lm(`Sepal Width` ~ Petal.Length + `a m` * log(Sepal.Length), data = iris)
}