test_that("item_omega", {
  skip_if_not_installed("psych")
  skip_if_not_installed("parameters")
  skip_if_not_installed("GPArotation")

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

  # reverse items
  omega2 <- item_omega(mtcars, n = 3, reverse_items = c("gear", "carb", "cyl"))
  expect_equal(
    as.numeric(omega),
    as.numeric(omega2),
    tolerance = 1e-3
  )

  # warning if items not found
  expect_warning(
    item_omega(mtcars, n = 3, reverse_items = c("gear", "carb", "cyl", "not_found")),
    "Following (variables)",
    fixed = TRUE
  )
})
