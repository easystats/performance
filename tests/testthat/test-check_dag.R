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
  dag <- check_dag(
    y ~ x + b + c + d,
    x ~ b,
    x ~ c,
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


test_that("check_dag, multiple adjustment sets", {
  dag <- check_dag(
    podcast ~ mood + humor + skills_course,
    alertness ~ mood,
    mood ~ humor,
    prepared ~ skills_course,
    exam ~ alertness + prepared,
    coords = ggdag::time_ordered_coords(),
    exposure = "podcast",
    outcome = "exam"
  )
  expect_snapshot(print(dag))
  dag <- check_dag(
    podcast ~ mood + humor + skills_course,
    alertness ~ mood,
    mood ~ humor,
    prepared ~ skills_course,
    exam ~ alertness + prepared,
    adjusted = c("alertness", "prepared"),
    exposure = "podcast",
    outcome = "exam",
    coords = ggdag::time_ordered_coords()
  )
  expect_snapshot(print(dag))
})


test_that("check_dag, different adjustements for total and direct", {
  dag <- check_dag(
    outcome ~ exposure + x1 + x2,
    x2 ~ exposure,
    exposure ~ x1,
    outcome = "outcome",
    exposure = "exposure"
  )
  expect_snapshot(print(dag))

  dag <- check_dag(
    outcome ~ exposure + x1 + x2,
    x2 ~ exposure,
    exposure ~ x1,
    adjusted = "x1",
    outcome = "outcome",
    exposure = "exposure"
  )
  expect_snapshot(print(dag))

  dag <- check_dag(
    outcome ~ exposure + x1 + x2,
    x2 ~ exposure,
    exposure ~ x1,
    adjusted = "x2",
    outcome = "outcome",
    exposure = "exposure"
  )
  expect_snapshot(print(dag))

  dag <- check_dag(
    outcome ~ exposure + x1 + x2,
    x2 ~ exposure,
    exposure ~ x1,
    adjusted = c("x1", "x2"),
    outcome = "outcome",
    exposure = "exposure"
  )
  expect_snapshot(print(dag))
})
