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
}
