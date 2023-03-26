requiet("ivreg")

test_that("Issue #530 from the `modelsummary` repo", {
  data(mtcars)
  iv_model <- ivreg(mpg ~ qsec + cyl + drat | disp | wt, data = mtcars)
  out <- expect_silent(model_performance(iv_model))

  expect_identical(
    capture.output(print(out)),
    c(
      "# Indices of model performance",
      "",
      "AIC     |     BIC |    R2 | R2 (adj.) |  RMSE | Sigma | Wu & Hausman | p (Wu_Hausman)",
      "-------------------------------------------------------------------------------------",
      "182.692 | 191.486 | 0.655 |     0.604 | 3.484 | 3.793 |       13.869 |         < .001"
    )
  )
})
