test_that("check_homogeneity | afex", {
  skip_if_not_installed("afex")

  data(obk.long, package = "afex")

  obk.long$treatment <- as.character(obk.long$treatment)
  suppressWarnings(suppressMessages({
    aM <- afex::aov_car(value ~ treatment * gender + Error(id / (phase * hour)),
      data = obk.long
    )

    aW <- afex::aov_car(value ~ Error(id / (phase * hour)),
      data = obk.long
    )

    aB <- afex::aov_car(value ~ treatment * gender + Error(id),
      data = obk.long
    )
  }))

  expect_error(check_homogeneity(aW))
  msg <- capture.output(expect_message(check_homogeneity(aB, method = "bartlett"), "Only"))

  msg <- capture.output({
    pM <- check_homogeneity(aM)
  })
  msg <- capture.output({
    pB <- check_homogeneity(aB)
  })

  expect_equal(pM, 0.3496516, ignore_attr = TRUE, tolerance = 0.001)
  expect_equal(pB, 0.3496516, ignore_attr = TRUE, tolerance = 0.001)
})

test_that("check_homogeneity | t-test", {
  data(mtcars)
  expect_error(
    check_homogeneity(t.test(mtcars$mpg, mtcars$hp, var.equal = FALSE)),
    regex = "Test does not assume"
  )

  out <- t.test(mtcars$mpg, mtcars$hp, var.equal = TRUE)
  expect_equal(
    check_homogeneity(out),
    structure(
      6.18792236963585e-121,
      object_name = out,
      method = "Bartlett Test",
      class = c(
        "check_homogeneity",
        "see_check_homogeneity", "numeric"
      )
    ),
    tolerance = 1e-3,
    ignore_attr = TRUE
  )
})
