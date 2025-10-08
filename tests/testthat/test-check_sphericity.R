test_that("check_sphericity | afex", {
  skip_if_not_installed("afex")

  data(obk.long, package = "afex")

  obk.long$treatment <- as.character(obk.long$treatment)
  suppressWarnings(suppressMessages({
    aM <- afex::aov_car(
      value ~ treatment * gender + Error(id / (phase * hour)),
      data = obk.long
    )

    aW <- afex::aov_car(value ~ Error(id / (phase * hour)), data = obk.long)

    aB <- afex::aov_car(value ~ treatment * gender + Error(id), data = obk.long)
  }))

  expect_error(check_sphericity(aB))

  msg <- capture.output(pM <- check_sphericity(aM))
  msg <- capture.output(pW <- check_sphericity(aW))

  expect_equal(length(pM), 12L)
  expect_equal(length(pW), 3L)
})
