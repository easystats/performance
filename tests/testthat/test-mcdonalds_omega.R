test_that("mcdonalds_omega, data frame", {
  data(mtcars)
  x <- mtcars[, c("cyl", "gear", "carb", "hp")]
  expect_warning(mcdonalds_omega(x), regex = "is not in range [0, 1]")
  expect_warning(mcdonalds_omega(x, ci = NULL), regex = "is greater than 1")
  expect_equal(mcdonalds_omega(x, verbose = FALSE), 1.156718, tolerance = 1e-3)

  data(iris)
  x <- iris[1:4]
  expect_equal(
    mcdonalds_omega(x),
    data.frame(
      Omega = 0.984746012592052,
      CI_low = 0.969115091775479,
      CI_high = 0.992527090611996
    ),
    tolerance = 1e-4,
    ignore_attr = TRUE
  )
  expect_equal(
    mcdonalds_omega(x, ci = NULL),
    0.984746012592052,
    tolerance = 1e-4,
    ignore_attr = TRUE
  )
  expect_equal(
    mcdonalds_omega(x, ci = 0.8),
    data.frame(
      Omega = 0.984746012592052,
      CI_low = 0.97577453015612,
      CI_high = 0.990427655221259
    ),
    tolerance = 1e-4,
    ignore_attr = TRUE
  )
})

test_that("mcdonalds_omega", {
  expect_warning(expect_null(mcdonalds_omega(mtcars[1])), regex = "Too few columns")
})


test_that("mcdonalds_omega, principal_components", {
  skip_if_not_installed("parameters", minimum_version = "0.21.3")
  pca <- parameters::principal_components(iris[1:4], n = 2)
  expect_equal(mcdonalds_omega(pca, verbose = FALSE), c(PC1 = 0.9855684), tolerance = 1e-3)
  expect_warning(mcdonalds_omega(pca), regex = "Too few columns")

  pca <- parameters::principal_components(iris[1:4], n = 1)
  expect_equal(mcdonalds_omega(pca, verbose = FALSE), c(PC1 = 0.984746), tolerance = 1e-3)
  expect_silent(mcdonalds_omega(pca))
})


test_that("mcdonalds_omega, principal_components", {
  skip_if_not_installed("parameters", minimum_version = "0.20.3")
  pca <- parameters::principal_components(mtcars, n = 2)
  expect_equal(mcdonalds_omega(pca), c(PC1 = 0.91522, PC2 = 0.0086), tolerance = 1e-3)
})


test_that("mcdonalds_omega, matrix", {
  m <- as.matrix(iris[1:4])
  expect_equal(
    mcdonalds_omega(x),
    data.frame(
      Omega = 0.984746012592052,
      CI_low = 0.969115091775479,
      CI_high = 0.992527090611996
    ),
    tolerance = 1e-4,
    ignore_attr = TRUE
  )
})
