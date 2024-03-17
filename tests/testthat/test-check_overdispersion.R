test_that("check_overdispersion, glmmTMB-poisson", {
  skip_if_not_installed("glmmTMB")
  skip_if_not(getRversion() >= "4.0.0")
  data(Salamanders, package = "glmmTMB")

  m1 <- glm(count ~ spp + mined, family = poisson, data = Salamanders)
  out <- check_overdispersion(m1)
  expect_equal(
    out,
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
  expect_identical(
    capture.output(print(out)),
    c(
      "# Overdispersion test",
      "",
      "       dispersion ratio =    2.946",
      "  Pearson's Chi-Squared = 1873.710",
      "                p-value =  < 0.001",
      ""
    )
  )
  expect_message(capture.output(print(out)), "Overdispersion detected")

  set.seed(123)
  out <- check_overdispersion(simulate_residuals(m1))
  expect_equal(
    out,
    structure(
      list(
        dispersion_ratio = 3.91516791651235,
        p_value = 0
      ),
      class = c("check_overdisp", "see_check_overdisp")
    ),
    tolerance = 1e-3
  )
})


test_that("check_overdispersion, glmmTMB-poisson mixed", {
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


test_that("check_overdispersion, zero-inflated and negbin", {
  skip_if_not_installed("glmmTMB")
  skip_if_not_installed("DHARMa")
  skip_if_not(getRversion() >= "4.0.0")
  data(Salamanders, package = "glmmTMB")

  m1 <- glmmTMB::glmmTMB(
    count ~ spp + mined,
    ziformula = ~ spp + mined,
    family = poisson,
    data = Salamanders
  )
  m2 <- glmmTMB::glmmTMB(
    count ~ spp + mined,
    family = poisson,
    data = Salamanders
  )
  m3 <- glmmTMB::glmmTMB(
    count ~ spp + mined,
    family = glmmTMB::nbinom1(),
    data = Salamanders
  )
  expect_equal(
    check_overdispersion(m1),
    structure(
      list(
        dispersion_ratio = 1.98057695890769,
        p_value = 0
      ),
      class = c("check_overdisp", "see_check_overdisp")
    ),
    tolerance = 1e-4,
    ignore_attr = TRUE
  )
  expect_equal(
    check_overdispersion(m2),
    structure(
      list(
        chisq_statistic = 1873.7105986433,
        dispersion_ratio = 2.94608584692342,
        residual_df = 636L,
        p_value = 3.26556213101505e-122
      ),
      class = c("check_overdisp", "see_check_overdisp"),
      object_name = "m1"
    ),
    tolerance = 1e-4,
    ignore_attr = TRUE
  )
  expect_equal(
    check_overdispersion(m1),
    structure(
      list(
        dispersion_ratio = 1.98057695890769,
        p_value = 0
      ),
      class = c("check_overdisp", "see_check_overdisp")
    ),
    tolerance = 1e-4,
    ignore_attr = TRUE
  )
})


test_that("check_overdispersion, MASS::negbin", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("DHARMa")
  set.seed(3)
  mu <- rpois(500, lambda = 3)
  x <- rnorm(500, mu, mu * 3)
  x <- ceiling(x)
  x <- pmax(x, 0)
  m <- MASS::glm.nb(x ~ mu)
  out <- check_overdispersion(m)
  expect_equal(
    out,
    structure(
      list(
        dispersion_ratio = 0.409521313173506,
        p_value = 0
      ),
      class = c("check_overdisp", "see_check_overdisp")
    ),
    ignore_attr = TRUE,
    tolerance = 1e-4
  )
  expect_identical(
    capture.output(print(out)),
    c(
      "# Overdispersion test",
      "",
      " dispersion ratio =   0.410",
      "          p-value = < 0.001",
      ""
    )
  )
  expect_message(capture.output(print(out)), "Underdispersion detected")

  # check that plot works
  skip_if_not_installed("see")
  expect_s3_class(plot(out), "ggplot")
})


test_that("check_overdispersion, genpois", {
  skip_if_not_installed("glmmTMB")
  skip_if_not_installed("DHARMa")
  skip_if_not(getRversion() >= "4.0.0")
  data(Salamanders, package = "glmmTMB")

  model <- glmmTMB::glmmTMB(
    count ~ mined + spp + (1 | site),
    family = glmmTMB::genpois(),
    data = Salamanders
  )
  expect_equal(
    check_overdispersion(model),
    structure(
      list(
        dispersion_ratio = 0.971975646955856,
        p_value = 0.88
      ),
      class = c("check_overdisp", "see_check_overdisp")
    ),
    tolerance = 1e-4,
    ignore_attr = TRUE
  )
})
