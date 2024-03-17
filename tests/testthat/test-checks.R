test_that("check_factorstructure", {
  skip_if_not_installed("parameters")
  x <- check_factorstructure(mtcars)
  expect_equal(x$KMO$MSA, 0.826, tolerance = 0.01)
  expect_equal(x$sphericity$chisq, 408.011, tolerance = 0.01)
})

test_that("check_clusterstructure, ok", {
  skip_if_not_installed("parameters")
  set.seed(333)
  out <- check_clusterstructure(iris[, 1:4])
  expect_equal(out$H, 0.187, tolerance = 0.01)
  expect_identical(
    capture.output(print(out)),
    c(
      "# Clustering tendency",
      "",
      "The dataset is suitable for clustering (Hopkins' H = 0.18)."
    )
  )
})

test_that("check_clusterstructure, bad", {
  skip_if_not_installed("parameters")
  set.seed(13)
  out <- check_clusterstructure(mtcars[, 10:11])
  expect_equal(out$H, 0.5142575, tolerance = 0.01)
  expect_identical(
    capture.output(print(out)),
    c(
      "# Clustering tendency",
      "",
      "The dataset is not suitable for clustering (Hopkins' H = 0.51)."
    )
  )
})
