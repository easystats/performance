if (requiet("parameters")) {
  data(mtcars)
  test_that("check_factorstructure", {
    x <- check_factorstructure(mtcars)
    expect_equal(x$KMO$MSA, 0.826, tolerance = 0.01)
    expect_equal(x$sphericity$chisq, 408.011, tolerance = 0.01)
  })

  test_that("check_clusterstructure", {
    set.seed(333)
    expect_equal(check_clusterstructure(iris[, 1:4])$H, 0.187, tolerance = 0.01)
  })
}
