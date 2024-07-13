skip_on_cran()
skip_if_not_installed("withr")
withr::with_options(
  list(easystats_errors = TRUE),
  test_that(".safe works with options", {
    expect_error(performance:::.safe(mean(fd)), regex = "object 'fd' not found")
    expect_identical(performance:::.safe(mean(fd), 1L), 1L)
    expect_identical(performance:::.safe(mean(c(1, 2, 3))), 2)
  })
)
test_that(".safe works", {
  expect_null(performance:::.safe(mean(fd)))
  expect_identical(performance:::.safe(mean(c(1, 2, 3))), 2)
})
