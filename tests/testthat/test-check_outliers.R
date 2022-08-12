if (requiet("testthat") && requiet("performance")) {

  test_that({

    expect_equal(
      check_outliers(mtcars$mpg, method = "zscore", threshold = -1),
      "Error: The `threshold` argument must be one or greater for method 'zscore'."
    )

    })
}
