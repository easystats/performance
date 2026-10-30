skip_if_not_installed("estimatr")
skip_if_not_installed("insight", minimum_version = "1.7.0")

test_that("model_performance.lm_robust", {
  data(mtcars)
  fit_fe <- estimatr::lm_robust(mpg ~ hp, data = mtcars, fixed_effects = ~cyl)
  fit_lm <- stats::lm(mpg ~ hp + factor(cyl), data = mtcars)
  out1 <- model_performance(fit_fe)
  out2 <- model_performance(fit_lm)
  expect_equal(
    as.data.frame(out1),
    as.data.frame(out2),
    tolerance = 1e-4,
    ignore_attr = TRUE
  )
})
