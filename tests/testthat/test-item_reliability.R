test_that("item_reliability", {
  data(iris)
  x <- iris[, 1:4]
  expect_equal(
    item_reliability(x),
    structure(
      list(
        term = c(
          "Sepal.Length", "Sepal.Width", "Petal.Length",
          "Petal.Width"
        ),
        alpha_if_deleted = c(0.454, 0.877, 0.489, 0.467),
        item_discrimination = c(0.894, -0.349, 0.863, 0.921)
      ),
      class = "data.frame",
      row.names = c(NA, -4L)
    ),
    tolerance = 1e-3
  )
})
