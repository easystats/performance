if (requiet("testthat") && requiet("performance")) {

  test_that({
    expect_error(
      check_outliers(mtcars$mpg, method = "zscore", threshold = -1),
      "The `threshold` argument"
    )
  })
}
