if (requiet("testthat") && requiet("performance")) {
  test_that("cronbachs_alpha", {
    data(mtcars)
    x <- mtcars[, c("cyl", "gear", "carb", "hp")]
    expect_equal(cronbachs_alpha(x), 0.09463206, tolerance = 1e-3)
  })
}


if (requiet("testthat") && requiet("performance") && requiet("parameters")) {
  test_that("cronbachs_alpha", {
    pca <- parameters::principal_components(mtcars[, c("cyl", "gear", "carb", "hp")], n = 1)
    expect_equal(cronbachs_alpha(pca), 0.09463206, tolerance = 1e-3)
  })
  test_that("cronbachs_alpha", {
    pca <- parameters::principal_components(mtcars, n = 2)
    expect_equal(cronbachs_alpha(pca), c(PC1 = 0.4396, PC2 = -1.44331), tolerance = 1e-3)
  })
}
