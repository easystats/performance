if (require("testthat") && require("performance")) {
  context("item_intercor")

  test_that("item_intercor", {
    data(iris)
    x <- iris[, 1:4]
    expect_equal(item_intercor(x), 0.2900708, tolerance = 1e-3)
    expect_equal(item_intercor(x, "spearman"), 0.3147349, tolerance = 1e-3)
  })
}
