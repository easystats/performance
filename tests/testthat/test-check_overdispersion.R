test_that("check_overdispersion", {
  skip_if_not_installed("glmmTMB")
  skip_if_not(getRversion() >= "4.0.0")
  data(Salamanders, package = "glmmTMB")

  m1 <- glm(count ~ spp + mined, family = poisson, data = Salamanders)
  expect_equal(
    check_overdispersion(m1),
    structure(
      list(
        chisq_statistic = 1873.71012423995,
        dispersion_ratio = 2.94608510100621,
        residual_df = 636L,
        p_value = 3.26607509162498e-122
      ),
      class = c("check_overdisp", "see_check_overdisp"),
      object_name = "m1"
    ),
    tolerance = 1e-3
  )
})

test_that("check_overdispersion", {
  skip_if_not_installed("glmmTMB")
  skip_if_not(getRversion() >= "4.0.0")
  data(Salamanders, package = "glmmTMB")

  m2 <- glmmTMB::glmmTMB(
    count ~ mined + spp + (1 | site),
    family = poisson,
    data = Salamanders
  )
  expect_equal(
    check_overdispersion(m2),
    structure(
      list(
        chisq_statistic = 1475.87512547128,
        dispersion_ratio = 2.32421279601777,
        residual_df = 635L,
        p_value = 8.41489530177729e-69
      ),
      class = c("check_overdisp", "see_check_overdisp"),
      object_name = "m2"
    ),
    tolerance = 1e-3
  )
})
