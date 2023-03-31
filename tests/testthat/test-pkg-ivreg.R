test_that("Issue #530 from the `modelsummary` repo", {
  skip_if_not_installed("ivreg")
  skip_if_not(packageVersion("insight") >= "0.19.1.3") # formatting of results
                                                      # for ivreg diagnostics
  data(mtcars)
  iv_model <- suppressMessages(ivreg::ivreg(mpg ~ qsec + cyl + drat | disp | wt, data = mtcars))
  out <- expect_silent(model_performance(iv_model))
  expect_snapshot(out)
})
