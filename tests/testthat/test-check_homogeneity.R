if (requiet("performance")) {
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

    msg <- capture.output(pM <- check_homogeneity(aM))
    msg <- capture.output(pB <- check_homogeneity(aB))

    expect_equal(pM, 0.3496516, ignore_attr = TRUE, tolerance = 0.001)
    expect_equal(pB, 0.3496516, ignore_attr = TRUE, tolerance = 0.001)
  })
}
