test_that("Issue #530 from the `modelsummary` repo", {
  skip_if_not_installed("ivreg")
  data(mtcars)
  iv_model <- suppressMessages(ivreg::ivreg(mpg ~ qsec + cyl + drat | disp | wt, data = mtcars))
  out <- expect_silent(model_performance(iv_model))
  expect_snapshot(out)
})
