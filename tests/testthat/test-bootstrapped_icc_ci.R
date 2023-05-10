test_that("bootstrapped icc ci_methods", {
  skip_on_cran()
  skip_on_os(c("mac", "linux"))
  skip_if_not_installed("lme4")

  data(sleepstudy, package = "lme4")
  m_icc <- lme4::lmer(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)

  set.seed(123)
  out1 <- icc(m_icc, ci = 0.95, iterations = 20)

  set.seed(123)
  out2 <- icc(m_icc, ci = 0.95, iterations = 20, ci_method = "boot")

  set.seed(123)
  out3 <- icc(m_icc, ci = 0.95, iterations = 20, ci_method = "analytical")

  expect_equal(
    out1$ICC_adjusted,
    c(0.72166, 0.50154, 0.79417),
    tolerance = 1e-3,
    ignore_attr = TRUE
  )
  expect_equal(
    out2$ICC_adjusted,
    c(0.72166, 0.64683, 0.88631),
    tolerance = 1e-3,
    ignore_attr = TRUE
  )
  expect_equal(
    out3$ICC_adjusted,
    c(0.72166, 0.64359, 0.78347),
    tolerance = 1e-3,
    ignore_attr = TRUE
  )

  expect_snapshot(print(out1))
  expect_snapshot(print(out2))
  expect_snapshot(print(out3))
})


test_that("bootstrapped r2_nakagawa ci_methods", {
  skip_on_cran()
  skip_on_os(c("mac", "linux"))
  skip_if_not_installed("lme4")

  data(sleepstudy, package = "lme4")
  m_icc <- lme4::lmer(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)

  set.seed(123)
  out1 <- r2_nakagawa(m_icc, ci = 0.95, iterations = 20)

  set.seed(123)
  out2 <- r2_nakagawa(m_icc, ci = 0.95, iterations = 20, ci_method = "boot")

  set.seed(123)
  out3 <- r2_nakagawa(m_icc, ci = 0.95, iterations = 20, ci_method = "analytical")

  expect_equal(
    out1$R2_marginal,
    c(`Marginal R2` = 0.27865, CI_low = 0.20403, CI_high = 0.39177),
    tolerance = 1e-3,
    ignore_attr = TRUE
  )
  expect_equal(
    out2$R2_marginal,
    c(`Marginal R2` = 0.27865, CI_low = 0.23123, CI_high = 0.30851),
    tolerance = 1e-3,
    ignore_attr = TRUE
  )
  expect_equal(
    out3$R2_marginal,
    c(`Marginal R2` = 0.27865, CI_low = 0.17018, CI_high = 0.39031),
    tolerance = 1e-3,
    ignore_attr = TRUE
  )

  expect_snapshot(print(out1))
  expect_snapshot(print(out2))
  expect_snapshot(print(out3))
})
