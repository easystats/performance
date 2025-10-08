skip_on_os("mac")
skip_on_cran()

test_that("check_predictions", {
  data(mtcars)
  model <- lm(mpg ~ disp, data = mtcars)
  set.seed(99)
  out <- check_predictions(model)

  expect_named(
    out,
    c(
      "sim_1",
      "sim_2",
      "sim_3",
      "sim_4",
      "sim_5",
      "sim_6",
      "sim_7",
      "sim_8",
      "sim_9",
      "sim_10",
      "sim_11",
      "sim_12",
      "sim_13",
      "sim_14",
      "sim_15",
      "sim_16",
      "sim_17",
      "sim_18",
      "sim_19",
      "sim_20",
      "sim_21",
      "sim_22",
      "sim_23",
      "sim_24",
      "sim_25",
      "sim_26",
      "sim_27",
      "sim_28",
      "sim_29",
      "sim_30",
      "sim_31",
      "sim_32",
      "sim_33",
      "sim_34",
      "sim_35",
      "sim_36",
      "sim_37",
      "sim_38",
      "sim_39",
      "sim_40",
      "sim_41",
      "sim_42",
      "sim_43",
      "sim_44",
      "sim_45",
      "sim_46",
      "sim_47",
      "sim_48",
      "sim_49",
      "sim_50",
      "y"
    )
  )
  expect_equal(
    out$sim_1,
    c(
      23.70112,
      24.56502,
      25.43419,
      20.40954,
      13.58266,
      20.72532,
      11.95366,
      25.14559,
      22.61286,
      18.48403,
      20.26737,
      21.2291,
      20.67149,
      10.07628,
      0.25886,
      10.64176,
      10.18407,
      20.68235,
      28.10115,
      27.55045,
      28.22301,
      18.94021,
      16.87727,
      14.05421,
      13.8378,
      28.13797,
      26.86451,
      23.90539,
      10.68719,
      28.17587,
      21.65853,
      26.07681
    ),
    tolerance = 1e-4
  )
})


test_that("check_predictions, glmmTMB", {
  skip_if_not_installed("glmmTMB")
  data(mtcars)
  model <- glmmTMB::glmmTMB(vs ~ disp, data = mtcars, family = binomial())
  set.seed(99)
  out <- check_predictions(model)

  expect_named(
    out,
    c(
      "sim_1",
      "sim_2",
      "sim_3",
      "sim_4",
      "sim_5",
      "sim_6",
      "sim_7",
      "sim_8",
      "sim_9",
      "sim_10",
      "sim_11",
      "sim_12",
      "sim_13",
      "sim_14",
      "sim_15",
      "sim_16",
      "sim_17",
      "sim_18",
      "sim_19",
      "sim_20",
      "sim_21",
      "sim_22",
      "sim_23",
      "sim_24",
      "sim_25",
      "sim_26",
      "sim_27",
      "sim_28",
      "sim_29",
      "sim_30",
      "sim_31",
      "sim_32",
      "sim_33",
      "sim_34",
      "sim_35",
      "sim_36",
      "sim_37",
      "sim_38",
      "sim_39",
      "sim_40",
      "sim_41",
      "sim_42",
      "sim_43",
      "sim_44",
      "sim_45",
      "sim_46",
      "sim_47",
      "sim_48",
      "sim_49",
      "sim_50",
      "y"
    )
  )
  expect_equal(
    out$sim_1,
    c(
      1,
      1,
      1,
      1,
      0,
      1,
      0,
      1,
      1,
      1,
      1,
      0,
      0,
      0,
      0,
      0,
      0,
      1,
      1,
      1,
      1,
      0,
      0,
      0,
      0,
      1,
      1,
      1,
      0,
      1,
      0,
      1
    ),
    tolerance = 1e-4
  )
  expect_true(attributes(out)$model_info$is_bernoulli)

  model <- glmmTMB::glmmTMB(vs ~ disp + (1 | cyl), data = mtcars, family = binomial())
  set.seed(99)
  out <- check_predictions(model)

  expect_equal(
    out$sim_1,
    c(
      0,
      1,
      1,
      0,
      0,
      0,
      0,
      1,
      1,
      0,
      1,
      0,
      0,
      0,
      0,
      0,
      0,
      1,
      1,
      1,
      1,
      0,
      0,
      0,
      0,
      1,
      1,
      1,
      0,
      1,
      0,
      0
    ),
    tolerance = 1e-4
  )
  expect_true(attributes(out)$model_info$is_bernoulli)
})


test_that("check_predictions, glm, binomial", {
  data(mtcars)
  set.seed(1)
  tot <- rep(10, 100)
  suc <- rbinom(100, prob = 0.9, size = tot)
  dat <- data.frame(tot, suc)
  dat$prop <- suc / tot

  mod1 <- glm(cbind(suc, tot - suc) ~ 1, family = binomial, data = dat)

  mod2 <- glm(prop ~ 1, family = binomial, data = dat, weights = tot)

  mod3 <- glm(cbind(suc, tot) ~ 1, family = binomial, data = dat)

  mod4 <- glm(am ~ 1, family = binomial, data = mtcars)

  set.seed(1)
  out1 <- check_predictions(mod1)
  set.seed(1)
  out2 <- check_predictions(mod2)
  set.seed(1)
  out3 <- check_predictions(mod3)
  set.seed(1)
  out4 <- check_predictions(mod4)

  expect_equal(head(out1$sim_1), c(1, 0.9, 0.9, 0.8, 1, 0.8), tolerance = 1e-4)
  expect_false(attributes(out1)$model_info$is_bernoulli)
  expect_equal(head(out2$sim_1), c(1, 0.9, 0.9, 0.8, 1, 0.8), tolerance = 1e-4)
  expect_false(attributes(out2)$model_info$is_bernoulli)
  expect_equal(
    head(out3$sim_1),
    c(0.4, 0.42105, 0.47368, 0.61111, 0.4, 0.61111),
    tolerance = 1e-3
  )
  expect_false(attributes(out3)$model_info$is_bernoulli)
  expect_equal(head(out4$sim_1), c(0, 0, 0, 1, 0, 1), tolerance = 1e-4)
  expect_true(attributes(out4)$model_info$is_bernoulli)
})


test_that("check_predictions, lm, ratio-response", {
  skip_if_not_installed("lme4")
  data(cbpp, package = "lme4")
  model1 <- lm(I(incidence / size) ~ period, data = cbpp)
  set.seed(123)
  out <- check_predictions(model1)
  expect_equal(
    head(out$y),
    c(0.14286, 0.25, 0.44444, 0, 0.13636, 0.05556),
    ignore_attr = TRUE,
    tolerance = 1e-4
  )
})


test_that("check_predictions, glmmTMB, proportion and cbind binomial", {
  skip_if_not_installed("glmmTMB")
  skip_if_not_installed("lme4")

  data("cbpp", package = "lme4")
  m1 <- glmmTMB::glmmTMB(
    incidence / size ~ period + herd,
    weights = size,
    family = binomial,
    data = cbpp
  )

  m2 <- glmmTMB::glmmTMB(
    cbind(incidence, size - incidence) ~ period + herd,
    weights = NULL,
    family = binomial,
    data = cbpp
  )

  cbpp <- transform(cbpp, prop = incidence / size)
  m3 <- glmmTMB::glmmTMB(
    prop ~ period + herd,
    weights = size,
    family = binomial,
    data = cbpp
  )

  X <- with(cbpp, cbind(incidence, size - incidence))
  cbpp$X <- X

  m4 <- glmmTMB::glmmTMB(
    X ~ period + herd,
    family = binomial,
    weights = NULL,
    data = cbpp
  )

  set.seed(123)
  out1 <- check_predictions(m1)

  set.seed(123)
  out2 <- check_predictions(m2)

  set.seed(123)
  out3 <- check_predictions(m3)

  set.seed(123)
  out4 <- check_predictions(m4)

  expect_equal(out1$y, out2$y, tolerance = 1e-4)
  expect_equal(out1$sim_1, out2$sim_1, tolerance = 1e-4)
  expect_equal(out1$sim_16, out2$sim_16, tolerance = 1e-4)
  expect_equal(out1$y, out3$y, tolerance = 1e-4)
  expect_equal(out1$sim_1, out3$sim_1, tolerance = 1e-4)
  expect_equal(out1$sim_16, out3$sim_16, tolerance = 1e-4)
  expect_equal(out1$y, out4$y, tolerance = 1e-4)
  expect_equal(out1$sim_1, out4$sim_1, tolerance = 1e-4)
  expect_equal(out1$sim_16, out4$sim_16, tolerance = 1e-4)
})
