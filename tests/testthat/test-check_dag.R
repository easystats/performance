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
  data(mtcars)
  m <- lm(mpg ~ wt + gear + disp + cyl, data = mtcars)
  dag <- check_dag(
    m,
    wt ~ disp + cyl,
    wt ~ am
  )
  dag
  expect_snapshot(print(dag))
})

test_that("check_dag, cylic error", {
  expect_error(
    check_dag(
      y ~ x + b + c + d,
      x ~ c + d,
      b ~ x,
      b ~ y,
      outcome = "y",
      exposure = "x",
      adjusted = "c"
    ),
    regex = "Model is cyclic"
  )
})
