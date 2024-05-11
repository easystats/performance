test_that("cronbachs_alpha, data frame", {
  skip_if_not_installed("parameters")

  data(mtcars)
  x <- mtcars[, c("cyl", "gear", "carb", "hp")]
  expect_equal(cronbachs_alpha(x), 0.09463206, tolerance = 1e-3)
})

test_that("cronbachs_alpha", {
  expect_message(expect_null(cronbachs_alpha(mtcars[1])), regex = "Too few")
})

test_that("cronbachs_alpha, principal_components", {
  skip_if_not_installed("parameters")

  pca <- parameters::principal_components(mtcars[, c("cyl", "gear", "carb", "hp")], n = 2)
  expect_equal(cronbachs_alpha(pca, verbose = FALSE), c(PC1 = 0.1101384), tolerance = 1e-3)
  expect_message(cronbachs_alpha(pca), regex = "Too few")

  pca <- parameters::principal_components(mtcars[, c("cyl", "gear", "carb", "hp")], n = 1)
  expect_equal(cronbachs_alpha(pca, verbose = FALSE), c(PC1 = 0.09463206), tolerance = 1e-3)
  expect_silent(cronbachs_alpha(pca))
})

test_that("cronbachs_alpha, principal_components", {
  skip_if_not_installed("parameters")

  pca <- parameters::principal_components(mtcars, n = 2)
  expect_equal(cronbachs_alpha(pca), c(PC1 = 0.4396, PC2 = -1.44331), tolerance = 1e-3)
})

test_that("cronbachs_alpha, matrix", {
  skip_if_not_installed("parameters")

  m <- as.matrix(mtcars[c("cyl", "gear", "carb", "hp")])
  expect_equal(cronbachs_alpha(m), 0.09463206, tolerance = 1e-3)
})
