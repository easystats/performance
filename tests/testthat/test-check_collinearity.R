if (require("testthat") && require("performance") && require("glmmTMB")) {
  data(Salamanders)
  m1 <- glmmTMB(count ~ spp + mined + (1 | site),
                ziformula = ~ spp,
                Salamanders,
                family = poisson())

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
    expect_null(check_collinearity(m1, component = "zero_inflated"))
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
}
