test_that("item_discrimination", {
  data(iris)
  x <- iris[, 1:4]
  expect_equal(
    item_discrimination(x),
    structure(list(Item = c(
      "Sepal.Length", "Sepal.Width", "Petal.Length",
      "Petal.Width"
    ), Discrimination = c(
      0.894040250011166, -0.348657747269071,
      0.863271047498634, 0.921040960149154
    )), class = c(
      "item_discrimination",
      "data.frame"
    ), row.names = c(NA, -4L)),
    tolerance = 1e-3
  )
})
