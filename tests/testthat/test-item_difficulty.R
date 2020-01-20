if (require("testthat") && require("performance")) {
  test_that("item_difficulty", {
    data(iris)
    x <- iris[, 1:4]
    expect_equal(
      item_difficulty(x),
      structure(
        list(
          item = c(
            "Sepal.Length", "Sepal.Width", "Petal.Length",
            "Petal.Width"
          ),
          difficulty = c(0.74, 0.69, 0.54, 0.48),
          ideal = c(0.56, 0.61, 0.57, 0.7)
        ),
        class = c("item_difficulty", "data.frame"),
        row.names = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
      ),
      tolerance = 1e-3
    )
  })
}
