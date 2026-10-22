test_that("multi-coefficient terms use dimension-adjusted GVIFs for display", {
  set.seed(1)
  dat <- expand.grid(
    strain = factor(c("SD", "WKY")),
    session_f = factor(1:12),
    replicate = 1:5
  )
  dat$y <- rnorm(nrow(dat))
  model <- lm(y ~ strain * session_f, data = dat)
  out <- check_collinearity(model, verbose = FALSE)

  # Raw results are preserved; only display values use the per-df scale.
  expect_equal(out$VIF, c(12, 2048, 13312))
  display <- attr(out, "data")
  expect_equal(display$df, c(1, 11, 11))
  expect_equal(display$VIF, c(12, 2, 13312^(1 / 11)))
  expect_equal(display$SE_factor, out$SE_factor)
  expect_equal(attr(out, "CI")$VIF_CI_low, out$VIF_CI_low^(1 / display$df))
  expect_equal(attr(out, "CI")$VIF_CI_high, out$VIF_CI_high^(1 / display$df))

  printed <- capture.output(print(out))
  low <- match("Low Correlation", printed)
  high <- match("High Correlation", printed)
  expect_length(grep("session_f", printed[seq.int(low, high - 1)]), 2)
  expect_length(grep("session_f", printed[seq.int(high, length(printed))]), 0)
  expect_false("Moderate Correlation" %in% printed)

  # check_model() must use the same values, grouping and confidence limits.
  panel <- .model_diagnostic_vif(model, verbose = FALSE)
  expect_equal(panel$x, out$Term)
  expect_equal(panel$y, display$VIF)
  expect_equal(panel$df, display$df)
  expect_identical(panel$group, c("high", "low", "low"))
  expect_equal(attr(panel, "CI"), attr(out, "CI"))
})


test_that("one-coefficient terms retain their VIFs and confidence limits", {
  model <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
  out <- check_collinearity(model)
  expect_equal(attr(out, "data")$df, rep(1, 4))
  expect_equal(attr(out, "data")$VIF, out$VIF)
  expect_equal(attr(out, "CI")$VIF_CI_low, out$VIF_CI_low)
  expect_equal(attr(out, "CI")$VIF_CI_high, out$VIF_CI_high)
  panel <- .model_diagnostic_vif(model)
  expect_equal(panel$y, out$VIF)
  expect_identical(panel$group, c("moderate", "moderate", "low", "moderate"))
})
