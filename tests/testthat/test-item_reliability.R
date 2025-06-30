test_that("item_reliability", {
  data(iris)
  x <- iris[, 1:4]
  expect_message(
    {
      out <- item_reliability(x)
    },
    regex = "Some of the values"
  )

  expect_equal(
    out$Alpha_if_deleted,
    c(0.454, 0.877, 0.489, 0.467)
    tolerance = 1e-3
  )
  expect_equal(
    out$Discrimination,
    c(0.894, -0.349, 0.863, 0.921)
    tolerance = 1e-3
  )
  expect_named(
    out,
    c("Item", "Alpha_if_deleted", "Item_Total_Correlation", "Discrimination")
  )
})
