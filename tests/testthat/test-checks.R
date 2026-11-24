test_that("check_factorstructure", {
  skip_if_not_installed("parameters")
  x <- check_factorstructure(mtcars)
  expect_equal(x$KMO$MSA, 0.8265536, tolerance = 0.01)
  expect_equal(x$sphericity$chisq, 408.0116, tolerance = 0.01)
})

test_that("check_clusterstructure, ok", {
  skip_if_not_installed("parameters")
  set.seed(333)
  out <- check_clusterstructure(iris[, 1:4])
  expect_equal(out$H, 0.1793898, tolerance = 0.01)
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
  expect_equal(out$H, 0.5633751, tolerance = 0.01)
  expect_identical(
    capture.output(print(out)),
    c(
      "# Clustering tendency",
      "",
      "The dataset is not suitable for clustering (Hopkins' H = 0.56)."
    )
  )
})

test_that("check_clusterstructure excludes self-distance when row 1 is sampled", {
  x2 <- data.frame(a = c(0, 1), b = c(0, 1))
  h_vals <- vapply(
    1:20,
    function(s) {
      set.seed(s)
      check_clusterstructure(x2, standardize = FALSE)$H
    },
    numeric(1)
  )
  expect_true(all(h_vals > 0))

  set.seed(42)
  out_man <- check_clusterstructure(iris[, 1:4], distance = "manhattan")
  expect_true(is.numeric(out_man$H) && out_man$H > 0 && out_man$H < 0.5)
})
