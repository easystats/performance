if (require("testthat") && require("performance")) {
  context("item_difficulty")

  test_that("item_difficulty", {
    data(iris)
    x <- iris[, 1:4]
    expect_equal(
      item_difficulty(x),
      list(
        item = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
        difficulty = c("Sepal.Length" = 0.74, "Sepal.Width" = 0.69, "Petal.Length" = 0.54, "Petal.Width" = 0.48),
        ideal = c("Sepal.Length" = 0.56, "Sepal.Width" = 0.61, "Petal.Length" = 0.57, "Petal.Width" = 0.70)
      ),
      tolerance = 1e-3)
  })
}