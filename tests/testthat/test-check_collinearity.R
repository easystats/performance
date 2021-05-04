if (require("testthat") && require("performance") && require("glmmTMB")) {
  data(Salamanders)
  m1 <- glmmTMB(count ~ spp + mined + (1 | site),
    ziformula = ~spp,
    Salamanders,
    family = poisson()
  )

  test_that("check_collinearity", {
    expect_equal(
      check_collinearity(m1, component = "conditional")$VIF,
      c(1.00037354840318, 1.00037354840318),
      tolerance = 1e-3
    )
    expect_equal(
      check_collinearity(m1, component = "all")$VIF,
      c(1.00037354840318, 1.00037354840318),
      tolerance = 1e-3
    )
    expect_null(check_collinearity(m1, component = "zero_inflated", verbose = FALSE))
  })

  m2 <- glmmTMB(
    count ~ spp + mined + cover + (1 | site),
    ziformula =  ~ spp + mined + cover,
    family = nbinom2,
    data = Salamanders
  )

  test_that("check_collinearity", {
    expect_equal(
      check_collinearity(m2, component = "conditional")$VIF,
      c(1.09015, 1.2343, 1.17832),
      tolerance = 1e-3
    )
    expect_equal(
      check_collinearity(m2, component = "all")$VIF,
      c(1.09015, 1.2343, 1.17832, 1.26914, 1, 1.26914),
      tolerance = 1e-3
    )
    expect_equal(
      check_collinearity(m2, component = "zero_inflated")$VIF,
      c(1.26914, 1, 1.26914),
      tolerance = 1e-3
    )
  })

  test_that("check_collinearity | afex", {
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

    expect_message(ccoM <- check_collinearity(aM))
    expect_message(ccoW <- check_collinearity(aW))
    expect_message(ccoB <- check_collinearity(aB))

    expect_equal(nrow(ccoM), 15)
    expect_equal(nrow(ccoW), 3)
    expect_equal(nrow(ccoB), 3)
  })
}
