if (requiet("testthat") && requiet("performance")) {
  test_that("item_split_half", {
    data(iris)
    x <- iris[, 1:4]
    expect_equal(
      item_split_half(x),
      list(
        splithalf = 0.7864936,
        spearmanbrown = 0.8804886
      ),
      tolerance = 1e-3
    )

    x <- iris[, 1:2]
    expect_equal(
      item_split_half(x),
      list(splithalf = -0.11757, spearmanbrown = -0.26647),
      tolerance = 1e-3
    )
  })
}
