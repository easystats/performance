test_that("item_omega", {
  skip_if_not_installed("psych")
  skip_if_not_installed("parameters")
  skip_if_not_installed("GPArotation")
  skip_if_not_installed("knitr")

  data(mtcars)
  omega <- item_omega(mtcars, n = 3)

  expect_snapshot(print(omega))
  expect_snapshot(print_md(omega))

  expect_equal(
    as.numeric(omega),
    c(
      Alpha = 0.88219,
      G.6 = 0.97254,
      `Omega (hierarchical)` = 0.56637,
      `Omega (asymptotic H)` = 0.58222,
      `Omega (total)` = 0.97277
    ),
    tolerance = 1e-3
  )

  expect_snapshot(parameters::model_parameters(omega))
  expect_snapshot(summary(omega))
})


test_that("item_omega-2", {
  skip_on_cran()
  skip_if_not_installed("psych")
  skip_if_not_installed("parameters")
  skip_if_not_installed("GPArotation")
  skip_if_not_installed("correlation")
  skip_if_not_installed("discovr")

  raq_items <- as.data.frame(discovr::raq)
  raq_items$id <- NULL

  raq_poly <- correlation::correlation(raq_items, method = "polychoric")
  raq_poly_mtx <- as.matrix(raq_poly) # store correlation matrix

  # needs n_obs
  expect_error(
    item_omega(raq_poly_mtx, n = 4),
    regex = "You provided a square matrix"
  )

  out1 <- item_omega(raq_poly_mtx, n = 4, n_obs = 2571)
  expect_identical(dim(out1), c(5L, 2L))
  expect_equal(
    out1$Coefficient,
    c(0.87548, 0.89137, 0.67522, 0.74806, 0.90264),
    tolerance = 1e-3
  )

  out2 <- item_omega(as.matrix(raq_items), n = 4)
  expect_equal(
    out2$Coefficient,
    c(0.86042, 0.87407, 0.66302, 0.7473, 0.88722),
    tolerance = 1e-3
  )
})
