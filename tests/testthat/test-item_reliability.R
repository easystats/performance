test_that("item_reliability", {
  data(iris)
  expect_equal(
    item_reliability(iris[1:4]),
    data.frame(
      term = c(
        "Sepal.Length", "Sepal.Width", "Petal.Length",
        "Petal.Width"
      ),
      alpha_if_deleted = c(0.454, 0.877, 0.489, 0.467),
      item_discrimination = c(0.894, -0.349, 0.863, 0.921),
      stringsAsFactors = FALSE
    ),
    tolerance = 1e-3,
    ignore_attr = TRUE
  )
})

test_that("item_reliability, omega", {
  data(iris)
  expect_equal(
    item_reliability(iris[1:3], type = "omega"),
    data.frame(
      term = c("Sepal.Length", "Sepal.Width", "Petal.Length"),
      omega_if_deleted = c(0.029, 0.82, 0.367),
      item_discrimination = c(0.914, -0.339, 0.602),
      stringsAsFactors = FALSE
    ),
    tolerance = 1e-3,
    ignore_attr = TRUE
  )
})
