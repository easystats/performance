if (require("testthat") && require("performance")) {
  test_that("cronbachs_alpha", {
    data(mtcars)
    x <- mtcars[, c("cyl", "gear", "carb", "hp")]
    expect_equal(cronbachs_alpha(x), 0.09463206, tolerance = 1e-3)
  })
}
