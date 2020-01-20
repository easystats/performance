if (require("testthat") && require("performance")) {
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
  })
}
