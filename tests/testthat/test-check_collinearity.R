# https://github.com/easystats/performance/pull/547
test_that("check_collinearity, correct order in print", {
  m <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
  out <- capture.output(print(check_collinearity(m, verbose = FALSE)))
  expect_identical(
    out,
    c(
      "# Check for Multicollinearity",
      "",
      "Low Correlation",
      "",
      " Term  VIF    VIF 95% CI adj. VIF Tolerance Tolerance 95% CI",
      " gear 1.53 [1.19,  2.51]     1.24      0.65     [0.40, 0.84]",
      "",
      "Moderate Correlation",
      "",
      " Term  VIF    VIF 95% CI adj. VIF Tolerance Tolerance 95% CI",
      "   wt 5.05 [3.21,  8.41]     2.25      0.20     [0.12, 0.31]",
      "  cyl 5.41 [3.42,  9.04]     2.33      0.18     [0.11, 0.29]",
      " disp 9.97 [6.08, 16.85]     3.16      0.10     [0.06, 0.16]"
    )
  )
})


test_that("check_collinearity, interaction", {
  m <- lm(mpg ~ wt * cyl, data = mtcars)
  expect_message(check_collinearity(m), regex = "Model has interaction terms")
})


test_that("check_collinearity", {
  skip_if_not_installed("glmmTMB")
  skip_if_not(getRversion() >= "4.0.0")

  data(Salamanders, package = "glmmTMB")

  m1 <- glmmTMB::glmmTMB(count ~ spp + mined + (1 | site),
    ziformula = ~spp,
    Salamanders,
    family = poisson()
  )
  expect_equal(
    suppressWarnings(check_collinearity(m1, component = "conditional", verbose = FALSE)$VIF),
    c(1.00037354840318, 1.00037354840318),
    tolerance = 1e-3
  )
  expect_equal(
    suppressWarnings(check_collinearity(m1, component = "all", verbose = FALSE)$VIF),
    c(1.00037354840318, 1.00037354840318),
    tolerance = 1e-3
  )
  expect_null(check_collinearity(m1, verbose = FALSE, component = "zero_inflated"))
})


test_that("check_collinearity", {
  skip_if_not_installed("glmmTMB")
  skip_if_not(getRversion() >= "4.0.0")

  data(Salamanders, package = "glmmTMB")

  m2 <- glmmTMB::glmmTMB(
    count ~ spp + mined + cover + (1 | site),
    ziformula = ~ spp + mined + cover,
    family = glmmTMB::nbinom2,
    data = Salamanders
  )

  expect_equal(
    suppressWarnings(check_collinearity(m2, component = "conditional", verbose = FALSE)$VIF),
    c(1.09015, 1.2343, 1.17832),
    tolerance = 1e-3
  )
  expect_equal(
    suppressWarnings(check_collinearity(m2, component = "conditional", verbose = FALSE)$VIF_CI_low),
    c(1.03392, 1.14674, 1.10105),
    tolerance = 1e-3
  )
  expect_equal(
    suppressWarnings(check_collinearity(m2, component = "all", verbose = FALSE)$VIF),
    c(1.09015, 1.2343, 1.17832, 1.26914, 1, 1.26914),
    tolerance = 1e-3
  )
  expect_equal(
    suppressWarnings(check_collinearity(m2, component = "all", verbose = FALSE)$VIF_CI_low),
    c(1.03392, 1.14674, 1.10105, 1.17565, 1, 1.17565),
    tolerance = 1e-3
  )
  expect_equal(
    suppressWarnings(check_collinearity(m2, component = "zero_inflated", verbose = FALSE)$VIF),
    c(1.26914, 1, 1.26914),
    tolerance = 1e-3
  )
  expect_equal(
    suppressWarnings(check_collinearity(m2, component = "zero_inflated", verbose = FALSE)$Tolerance_CI_high),
    c(0.85059, 1, 0.85059),
    tolerance = 1e-3
  )

  suppressWarnings(coll <- check_collinearity(m2, component = "all", verbose = FALSE)) # nolint
  expect_true(all(coll$Tolerance < coll$Tolerance_CI_high))
  expect_true(all(coll$VIF > coll$VIF_CI_low))

  expect_identical(
    attributes(coll)$data$Component,
    c("conditional", "conditional", "conditional", "zero inflated", "zero inflated", "zero inflated")
  )
  expect_identical(
    colnames(attributes(coll)$CI),
    c("VIF_CI_low", "VIF_CI_high", "Tolerance_CI_low", "Tolerance_CI_high", "Component")
  )
})

test_that("check_collinearity | afex", {
  skip_if_not_installed("afex", minimum_version = "1.0.0")

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

  expect_message(ccoM <- check_collinearity(aM)) # nolint
  expect_warning(expect_message(ccoW <- check_collinearity(aW))) # nolint
  expect_message(ccoB <- check_collinearity(aB), regexp = NA) # nolint

  expect_identical(nrow(ccoM), 15L)
  expect_identical(nrow(ccoW), 3L)
  expect_identical(nrow(ccoB), 3L)

  suppressWarnings(suppressMessages({
    aM <- afex::aov_car(value ~ treatment * gender + Error(id / (phase * hour)),
      include_aov = TRUE,
      data = obk.long
    )

    aW <- afex::aov_car(value ~ Error(id / (phase * hour)),
      include_aov = TRUE,
      data = obk.long
    )

    aB <- afex::aov_car(value ~ treatment * gender + Error(id),
      include_aov = TRUE,
      data = obk.long
    )
  }))

  expect_message(ccoM <- check_collinearity(aM)) # nolint
  expect_warning(expect_message(ccoW <- check_collinearity(aW))) # nolint
  expect_message(ccoB <- check_collinearity(aB), regexp = NA) # nolint

  expect_identical(nrow(ccoM), 15L)
  expect_identical(nrow(ccoW), 3L)
  expect_identical(nrow(ccoB), 3L)
})

test_that("check_collinearity, ci = NULL", { # 518
  data(npk)
  m <- lm(yield ~ N + P + K, npk)
  out <- check_collinearity(m, ci = NULL)

  expect_identical(
    colnames(out),
    c(
      "Term", "VIF", "VIF_CI_low", "VIF_CI_high", "SE_factor", "Tolerance",
      "Tolerance_CI_low", "Tolerance_CI_high"
    )
  )
  expect_snapshot(out)
})

test_that("check_collinearity, ci are NA", {
  skip_if_not_installed("fixest")
  data(mtcars)
  i <- fixest::i
  m_vif <- fixest::feols(mpg ~ disp + hp + wt + i(cyl) | carb, data = mtcars)
  out <- suppressWarnings(check_collinearity(m_vif))
  expect_identical(
    colnames(out),
    c(
      "Term", "VIF", "VIF_CI_low", "VIF_CI_high", "SE_factor", "Tolerance",
      "Tolerance_CI_low", "Tolerance_CI_high"
    )
  )
})

test_that("check_collinearity, hurdle/zi models w/o zi-formula", {
  skip_if_not_installed("pscl")
  data("bioChemists", package = "pscl")
  m <- pscl::hurdle(
    art ~ fem + mar,
    data = bioChemists,
    dist = "poisson",
    zero.dist = "binomial",
    link = "logit"
  )
  out <- check_collinearity(m)
  expect_named(
    out,
    c(
      "Term", "VIF", "VIF_CI_low", "VIF_CI_high", "SE_factor", "Tolerance",
      "Tolerance_CI_low", "Tolerance_CI_high", "Component"
    )
  )
  expect_equal(out$VIF, c(1.05772, 1.05772, 1.06587, 1.06587), tolerance = 1e-4)
  expect_snapshot(print(out))
})

test_that("check_collinearity, invalid data", {
  dd <- data.frame(y = as.difftime(0:5, units = "days"))
  m1 <- lm(y ~ 1, data = dd)
  expect_message(check_collinearity(m1), "Could not extract the variance-covariance")
})

test_that("check_collinearity, glmmTMB hurdle w/o zi", {
  skip_if_not_installed("glmmTMB")
  data(Salamanders, package = "glmmTMB")
  mod_trunc_error <- glmmTMB::glmmTMB(
    count ~ spp + mined + (1 | site),
    data = Salamanders[Salamanders$count > 0, , drop = FALSE],
    family = glmmTMB::truncated_nbinom2(),
    ziformula = ~0,
    dispformula = ~1
  )
  out <- check_collinearity(mod_trunc_error)
  expect_equal(out$VIF, c(1.03414, 1.03414), tolerance = 1e-3)
})

test_that("check_collinearity, validate adjusted vif against car", {
  skip_if_not_installed("car")
  data(mtcars)
  mod <- lm(mpg ~ cyl + hp + am, data = mtcars)
  out1 <- car::vif(mod)
  out2 <- check_collinearity(mod)
  expect_equal(out1, out2$VIF, tolerance = 1e-3, ignore_attr = TRUE)

  mod <- lm(mpg ~ as.factor(cyl) + hp + gear, data = mtcars)
  out1 <- car::vif(mod)
  out2 <- check_collinearity(mod)
  expect_equal(out1[, 1], out2$VIF, tolerance = 1e-3, ignore_attr = TRUE)
  expect_equal(out1[, 3], out2$SE_factor, tolerance = 1e-3, ignore_attr = TRUE)

  mtcars$gear <- factor(mtcars$gear)
  mod <- lm(mpg ~ as.factor(cyl) + hp + gear, data = mtcars)
  out1 <- car::vif(mod)
  out2 <- check_collinearity(mod)
  expect_equal(out1[, 1], out2$VIF, tolerance = 1e-3, ignore_attr = TRUE)
  expect_equal(out1[, 3], out2$SE_factor, tolerance = 1e-3, ignore_attr = TRUE)
})
