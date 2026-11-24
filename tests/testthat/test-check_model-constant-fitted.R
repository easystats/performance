skip_if_not_installed("see", minimum_version = "0.9.1")

test_that("check_model skips fitted-value panels for one-sample models", {
  response <- rep(c(-1e12, 1e12), 16)
  model <- lm(response ~ 1)
  checks <- check_model(
    model,
    check = c("qq", "linearity", "homogeneity"),
    verbose = FALSE
  )

  expect_named(checks, "QQ")
})

test_that("check_model retains fitted-value panels when predictions vary", {
  d <- data.frame(response = (1:10)^2 * 1e-20, x = 1:10)
  model <- lm(response ~ x, data = d)
  checks <- check_model(model, check = c("ncv", "homogeneity"), verbose = FALSE)
  expect_named(checks, c("NCV", "HOMOGENEITY"))

  offset_model <- lm(response ~ 1, offset = x * 1e-20, data = d)
  checks <- check_model(
    offset_model,
    check = c("linearity", "homogeneity"),
    verbose = FALSE
  )
  expect_named(checks, c("NCV", "HOMOGENEITY"))
})
