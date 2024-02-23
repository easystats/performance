test_that("check_normality | afex", {
  skip_if_not(getRversion() >= "4.0.0")
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

  msg <- capture.output({
    pM <- check_normality(aM)
  })
  msg <- capture.output({
    pW <- check_normality(aW)
  })
  msg <- capture.output({
    pB <- check_normality(aB)
  })

  expect_equal(pM, 0.2054236, ignore_attr = TRUE, tolerance = 0.001)
  expect_equal(pW, 0.5496325, ignore_attr = TRUE, tolerance = 0.001)
  expect_equal(pB, 0.734765, ignore_attr = TRUE, tolerance = 0.001)
})

test_that("check_normality | glmmTMB", {
  skip_if_not_installed("glmmTMB")
  skip_if_not(getRversion() >= "4.0.0")

  data("Salamanders", package = "glmmTMB")
  m <- glmmTMB::glmmTMB(
    count ~ mined + spp + (1 | site),
    family = poisson,
    data = Salamanders
  )

  out <- check_normality(m, effects = "random")
  expect_identical(attributes(out)$re_groups, "site: (Intercept)")
  expect_equal(as.vector(out), 0.698457693553405, tolerance = 1e-3)

  expect_message(
    {
      out <- check_normality(m, effects = "fixed")
    },
    "for linear models"
  )
  expect_null(out)
})


test_that("check_normality | t-test", {
  out <- t.test(mtcars$mpg, mtcars$hp, var.equal = TRUE)
  expect_equal(
    check_normality(out),
    structure(
      7.15789362314837e-12,
      type = "residuals",
      object_name = out,
      effects = "fixed",
      class = c(
        "check_normality",
        "see_check_normality", "numeric"
      )
    ),
    tolerance = 1e-3,
    ignore_attr = TRUE
  )
})
