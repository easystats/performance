skip_on_cran()
skip_if_not_installed("lavaan")

test_that("model_performance.lavaan", {
  data(HolzingerSwineford1939, package = "lavaan")
  structure <- " visual  =~ x1 + x2 + x3
                textual =~ x4 + x5 + x6
                speed   =~ x7 + x8 + x9 "
  model <- lavaan::cfa(structure, data = HolzingerSwineford1939)
  out <- expect_silent(model_performance(model))
  expect_named(
    out,
    c(
      "Chi2",
      "Chi2_df",
      "p_Chi2",
      "Baseline",
      "Baseline_df",
      "p_Baseline",
      "GFI",
      "AGFI",
      "NFI",
      "NNFI",
      "CFI",
      "RMSEA",
      "RMSEA_CI_low",
      "RMSEA_CI_high",
      "p_RMSEA",
      "RMR",
      "SRMR",
      "RFI",
      "PNFI",
      "IFI",
      "RNI",
      "Loglikelihood",
      "AIC",
      "BIC",
      "BIC_adjusted"
    )
  )

  out <- model_performance(model, metrics = c("Chi2", "RMSEA"))
  expect_named(out, c("Chi2", "RMSEA"))

  model <- suppressWarnings(lavaan::cfa(structure, data = HolzingerSwineford1939[1:10, ]))
  expect_warning(
    {
      out <- model_performance(model)
    },
    regex = "This lavaan model did not converge"
  )
  expect_identical(out$Chi2, NA)
  expect_silent(model_performance(model, verbose = FALSE))
})
