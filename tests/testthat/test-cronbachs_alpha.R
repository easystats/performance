if (requiet("performance")) {
  test_that("cronbachs_alpha, data frame", {
    data(mtcars)
    x <- mtcars[, c("cyl", "gear", "carb", "hp")]
    expect_equal(cronbachs_alpha(x), 0.09463206, tolerance = 1e-3)
  })
  test_that("cronbachs_alpha", {
    expect_warning(expect_null(cronbachs_alpha(mtcars[1])))
  })
}


if (requiet("parameters")) {
  test_that("cronbachs_alpha, principal_components", {
    pca <- parameters::principal_components(mtcars[, c("cyl", "gear", "carb", "hp")], n = 1)
    expect_equal(cronbachs_alpha(pca), c(PC1 = 0.09463206), tolerance = 1e-3)
  })
  test_that("cronbachs_alpha, principal_components", {
    pca <- parameters::principal_components(mtcars, n = 2)
    expect_equal(cronbachs_alpha(pca), c(PC1 = 0.4396, PC2 = -1.44331), tolerance = 1e-3)
  })
}


if (requiet("performance")) {
  test_that("cronbachs_alpha, matrix", {
    m <- as.matrix(mtcars[c("cyl", "gear", "carb", "hp")])
    expect_equal(cronbachs_alpha(m), 0.09463206, tolerance = 1e-3)
  })
}
