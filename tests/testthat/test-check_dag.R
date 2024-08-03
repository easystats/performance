skip_on_cran()
skip_if_not_installed("ggdag")
skip_if_not_installed("dagitty")

test_that("check_dag", {
  dag <- check_dag(
    y ~ x + b,
    outcome = "y",
    exposure = "x"
  )
  expect_snapshot(print(dag))
  dag <- check_dag(
    y ~ x + b,
    outcome = "y",
    exposure = "x",
    adjusted = "b"
  )
  expect_snapshot(print(dag))
  dag <- check_dag(
    y ~ x + b + c,
    x ~ b,
    outcome = "y",
    exposure = "x"
  )
  expect_snapshot(print(dag))
  dag <- check_dag(
    y ~ x + b + c,
    x ~ b,
    outcome = "y",
    exposure = "x",
    adjusted = "c"
  )
  expect_snapshot(print(dag))
})
