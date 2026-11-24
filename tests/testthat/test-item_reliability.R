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
    c(0.454, 0.877, 0.489, 0.467),
    tolerance = 1e-3
  )
  expect_equal(
    out$Discrimination,
    c(0.894, -0.349, 0.863, 0.921),
    tolerance = 1e-3
  )
  expect_named(
    out,
    c("Item", "Alpha_if_deleted", "Item_Total_Correlation", "Discrimination")
  )
})

test_that("item_reliability standardize = TRUE works on data.frame and matrix", {
  data(mtcars)
  x_df <- mtcars[, c("cyl", "disp", "hp", "wt")]
  x_mat <- as.matrix(x_df)
  x_scaled <- as.data.frame(scale(x_df))

  out_df <- item_reliability(x_df, standardize = TRUE, verbose = FALSE)
  out_mat <- item_reliability(x_mat, standardize = TRUE, verbose = FALSE)
  out_ref <- item_reliability(x_scaled, standardize = FALSE, verbose = FALSE)

  expect_equal(out_df, out_ref, tolerance = 1e-6)
  expect_equal(out_mat, out_ref, tolerance = 1e-6, ignore_attr = TRUE)
  expect_equal(
    attr(out_mat, "cronbachs_alpha"),
    attr(out_ref, "cronbachs_alpha"),
    tolerance = 1e-6
  )
  expect_false(isTRUE(all.equal(
    out_df$Alpha_if_deleted,
    item_reliability(x_df, standardize = FALSE, verbose = FALSE)$Alpha_if_deleted
  )))
})
