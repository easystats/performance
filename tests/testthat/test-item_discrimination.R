test_that("item_discrimination", {
  data(iris)
  x <- iris[, 1:4]

  expect_message(item_discrimination(x), regex = "Some of the values")
  expect_silent(item_discrimination(x, verbose = FALSE))

  expect_equal(
    item_discrimination(x),
    structure(
      list(
        Item = c(
          "Sepal.Length",
          "Sepal.Width",
          "Petal.Length",
          "Petal.Width"
        ),
        Discrimination = c(
          0.894040250011166,
          -0.348657747269071,
          0.863271047498634,
          0.921040960149154
        )
      ),
      class = c(
        "item_discrimination",
        "data.frame"
      ),
      row.names = c(NA, -4L)
    ),
    tolerance = 1e-3
  )

  out1 <- item_discrimination(x, corrected = FALSE)
  out2 <- item_totalcor(x)
  expect_equal(
    out1$Item_Total_Correlation,
    out2$Item_Total_Correlation,
    tolerance = 1e-3
  )
})

test_that("item_discrimination standardize = TRUE works on data.frame and matrix", {
  data(mtcars)
  x_df <- mtcars[, c("cyl", "disp", "hp", "wt")]
  x_mat <- as.matrix(x_df)
  x_scaled <- as.data.frame(scale(x_df))

  out_df <- item_discrimination(x_df, standardize = TRUE, verbose = FALSE)
  out_mat <- item_discrimination(x_mat, standardize = TRUE, verbose = FALSE)
  out_ref <- item_discrimination(x_scaled, standardize = FALSE, verbose = FALSE)

  expect_equal(out_df, out_ref, tolerance = 1e-6)
  expect_equal(out_mat, out_ref, tolerance = 1e-6)
  expect_false(isTRUE(all.equal(
    out_df$Discrimination,
    item_discrimination(x_df, standardize = FALSE, verbose = FALSE)$Discrimination
  )))
})
