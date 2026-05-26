skip_if_not_installed("DHARMa", minimum_version = "0.5.0")

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
      class = c("check_overdisp", "see_check_overdisp"),
      simulated = TRUE
    ),
    tolerance = 1e-3
  )
})


test_that("check_overdispersion, glmmTMB-poisson mixed", {
  skip_if_not_installed("glmmTMB")
  skip_if_not(getRversion() >= "4.0.0")
  skip_if_not_installed("DHARMa", minimum_version = "0.5.0")

  data(Salamanders, package = "glmmTMB")

  m2 <- glmmTMB::glmmTMB(
    count ~ mined + spp + (1 | site),
    family = poisson,
    data = Salamanders
  )
  set.seed(123)
  expect_equal(
    check_overdispersion(m2, residual_type = "normal"),
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
  set.seed(123)
  expect_equal(
    check_overdispersion(m2),
    structure(
      list(dispersion_ratio = 3.04011005020607, p_value = 0),
      class = c("check_overdisp", "see_check_overdisp"),
      object_name = "m2",
      simulated = TRUE
    ),
    tolerance = 1e-3
  )
})


test_that("check_overdispersion, zero-inflated and negbin", {
  skip_if_not_installed("glmmTMB")
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

  set.seed(123)
  expect_equal(
    check_overdispersion(m1),
    structure(
      list(
        dispersion_ratio = 1.98057695890769,
        p_value = 0
      ),
      class = c("check_overdisp", "see_check_overdisp"),
      simulated = TRUE
    ),
    tolerance = 1e-4,
    ignore_attr = TRUE
  )

  set.seed(123)
  expect_equal(
    check_overdispersion(m1, residual_type = "normal"),
    structure(
      list(
        chisq_statistic = 1027.18155565511,
        dispersion_ratio = 1.63563941983298,
        residual_df = 628L,
        p_value = 9.12825627359497e-22
      ),
      class = c("check_overdisp", "see_check_overdisp"),
      object_name = "m1"
    ),
    tolerance = 1e-4,
    ignore_attr = TRUE
  )

  set.seed(123)
  expect_equal(
    check_overdispersion(m2, residual_type = "normal"),
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
  set.seed(123)
  expect_equal(
    check_overdispersion(m2),
    structure(
      list(dispersion_ratio = 3.91516799008681, p_value = 0),
      class = c("check_overdisp", "see_check_overdisp"),
      object_name = "m2",
      simulated = TRUE
    ),
    tolerance = 1e-4,
    ignore_attr = TRUE
  )

  set.seed(123)
  expect_equal(
    check_overdispersion(m3, residual_type = "normal"),
    structure(
      list(
        chisq_statistic = 544.29641690291,
        dispersion_ratio = 0.857159711658125,
        residual_df = 635L,
        p_value = 0.996077528983478
      ),
      class = c("check_overdisp", "see_check_overdisp"),
      object_name = "m3"
    ),
    tolerance = 1e-4,
    ignore_attr = TRUE
  )
  set.seed(123)
  expect_equal(
    check_overdispersion(m3),
    structure(
      list(dispersion_ratio = 1.18027855021855, p_value = 0.232),
      class = c("check_overdisp", "see_check_overdisp"),
      object_name = "m3",
      simulated = TRUE
    ),
    tolerance = 1e-4,
    ignore_attr = TRUE
  )
})


test_that("check_overdispersion, MASS::negbin", {
  skip_if_not_installed("MASS")
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
      "# Overdispersion test (using simulated residuals)",
      "",
      " dispersion ratio =   0.410",
      "          p-value = < 0.001",
      ""
    )
  )
  expect_message(capture.output(print(out)), "Underdispersion detected")

  # check that plot works
  skip_if_not_installed("see", minimum_version = "0.9.1")
  expect_s3_class(plot(out), "ggplot")
})


test_that("check_overdispersion, genpois", {
  skip_if_not_installed("glmmTMB")
  skip_if_not(getRversion() >= "4.0.0")
  data(Salamanders, package = "glmmTMB")

  model <- glmmTMB::glmmTMB(
    count ~ mined + spp + (1 | site),
    family = glmmTMB::genpois(),
    data = Salamanders
  )
  set.seed(123)
  expect_equal(
    check_overdispersion(model),
    structure(
      list(dispersion_ratio = 1.13005481966618, p_value = 0.408),
      class = c("check_overdisp", "see_check_overdisp"),
      object_name = "model",
      simulated = TRUE
    ),
    tolerance = 1e-4
  )
  set.seed(123)
  expect_equal(
    check_overdispersion(model, residual_type = "normal"),
    structure(
      list(
        chisq_statistic = 473.48007461303,
        dispersion_ratio = 0.74681399781235,
        residual_df = 634L,
        p_value = 0.999999604566096
      ),
      class = c("check_overdisp", "see_check_overdisp"),
      object_name = "model"
    ),
    tolerance = 1e-4
  )
})


test_that("check_overdispersion, glm can use normal residuals", {
  #fmt: skip
  dat <- data.frame(
    n = c(
      55, 59, 74, 7, 54, 54, 57, 48, 55, 57, 41,
      20, 13, 21, 13, 32, 38, 42, 37, 14, 19
    ),
    success = c(
      26, 35, 28, 6, 16, 35, 28, 21, 31, 10, 2, 9, 7,
      18, 1, 26, 28, 3, 17, 11, 17
    ),
    x = c(
      3.464, 1.599, 3.39, 3.047, 2.442, 1.777, 3.363,
      4.626, 2.701, 4.636, 3.622, 2.031, 1.666, 2.218, 3.338, 4.255,
      2.476, 4.727, 3.317, 3.925, 2.854
    )
  )

  mod1 <- glm(cbind(success, n - success) ~ x, data = dat, family = binomial)

  set.seed(123)
  out <- check_overdispersion(mod1, residual_type = "normal")
  expect_equal(
    out,
    structure(
      list(
        chisq_statistic = 105583.222033006,
        dispersion_ratio = 5557.01168594766,
        residual_df = 19L,
        p_value = 0
      ),
      class = c("check_overdisp", "see_check_overdisp"),
      object_name = "mod1"
    ),
    tolerance = 1e-4
  )

  set.seed(123)
  out <- check_overdispersion(mod1)
  expect_equal(
    out,
    structure(
      list(dispersion_ratio = 1.07771876659311, p_value = 0.528),
      class = c("check_overdisp", "see_check_overdisp"),
      object_name = "mod1",
      simulated = TRUE
    ),
    tolerance = 1e-4
  )
})
