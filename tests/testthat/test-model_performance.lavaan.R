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

test_that("model_performance.lavaan passes arguments to fitmeasures", {
  data(HolzingerSwineford1939, package = "lavaan")
  structure <- " visual  =~ x1 + x2 + x3
                textual =~ x4 + x5 + x6
                speed   =~ x7 + x8 + x9 "
  model <- lavaan::cfa(structure, data = HolzingerSwineford1939)
  # lavaan >= 0.7 names the argument "fm_args", older versions "fm.args"
  fm_name <- if ("fm_args" %in% names(formals(lavaan::fitmeasures))) {
    "fm_args"
  } else {
    "fm.args"
  }
  fm_args <- stats::setNames(list(list(rmsea.ci.level = 0.5)), fm_name)
  out <- do.call(model_performance, c(list(model), fm_args))
  expected <- do.call(lavaan::fitmeasures, c(list(model), fm_args))
  expect_identical(out$RMSEA_CI_low, unname(expected[["rmsea.ci.lower"]]))

  measures <- lavaan::fitmeasures(model)
  agfi <- if ("agfi" %in% names(measures)) "agfi" else "agfi_lisrel"
  expect_equal(out$AGFI, unname(measures[[agfi]]))
})

test_that("model_performance.lavaan, fit measures without likelihood", {
  data(HolzingerSwineford1939, package = "lavaan")
  structure <- " visual  =~ x1 + x2 + x3
                textual =~ x4 + x5 + x6
                speed   =~ x7 + x8 + x9 "
  ordinal <- as.data.frame(lapply(
    HolzingerSwineford1939[paste0("x", 1:9)],
    cut,
    breaks = 3,
    labels = FALSE
  ))
  model <- lavaan::cfa(structure, data = ordinal, ordered = TRUE)
  out <- model_performance(model)
  # ordered indicators use the WLSMV estimator, which has a robust test
  expect_named(out, c(unname(.lavaan_fit_map()), unname(.lavaan_robust_fit_map())))
  expect_identical(out$Loglikelihood, NA_real_)
  expect_identical(out$CFI_robust, unname(lavaan::fitMeasures(model)[["cfi.robust"]]))
})

test_that("model_performance.lavaan, robust estimators", {
  data(HolzingerSwineford1939, package = "lavaan")
  structure <- " visual  =~ x1 + x2 + x3
                textual =~ x4 + x5 + x6
                speed   =~ x7 + x8 + x9 "
  # columns and the lavaan::fitMeasures() entries they come from
  robust <- c(
    Chi2_scaled = "chisq.scaled",
    Chi2_scaled_df = "df.scaled",
    p_Chi2_scaled = "pvalue.scaled",
    CFI_robust = "cfi.robust",
    NNFI_robust = "tli.robust",
    RMSEA_robust = "rmsea.robust",
    RMSEA_robust_CI_low = "rmsea.ci.lower.robust",
    RMSEA_robust_CI_high = "rmsea.ci.upper.robust",
    p_RMSEA_robust = "rmsea.pvalue.robust"
  )

  for (estimator in c("MLR", "MLM")) {
    model <- lavaan::cfa(structure, data = HolzingerSwineford1939, estimator = estimator)
    measures <- lavaan::fitMeasures(model)
    out <- expect_silent(model_performance(model))
    expect_named(out, c(unname(.lavaan_fit_map()), names(robust)))
    for (i in names(robust)) {
      expect_identical(out[[i]], unname(measures[[robust[[i]]]]), info = i)
    }
    # the standard indices are unchanged
    expect_identical(out$Chi2, unname(measures[["chisq"]]))
    expect_identical(out$CFI, unname(measures[["cfi"]]))
  }

  # "robust" selects only the scaled and robust indices
  out <- model_performance(model, metrics = "robust")
  expect_named(out, names(robust))
  out <- model_performance(model, metrics = c("CFI", "robust", "SRMR"))
  expect_named(out, c("CFI", names(robust), "SRMR"))
  out <- model_performance(model, metrics = c("CFI", "CFI_robust"))
  expect_named(out, c("CFI", "CFI_robust"))

  # the scaled test is printed like the standard one
  printed <- capture.output(print(model_performance(model, metrics = "robust")))
  expect_true(any(grepl("Chi2_scaled(24)", printed, fixed = TRUE)))
  expect_true(any(grepl("p (Chi2_scaled)", printed, fixed = TRUE)))
  expect_true(any(grepl("p (RMSEA_robust)", printed, fixed = TRUE)))
  expect_true(any(grepl("< .001", printed, fixed = TRUE)))

  # models without a robust test statistic have no robust indices
  model <- lavaan::cfa(structure, data = HolzingerSwineford1939)
  expect_named(model_performance(model), unname(.lavaan_fit_map()))
  out <- model_performance(model, metrics = "robust")
  expect_named(out, names(robust))
  expect_true(all(is.na(unlist(out))))
})


# lavaan.mi ------------------------------------------------------------------

lavaan_mi_structure <- " visual  =~ x1 + x2 + x3
                         textual =~ x4 + x5 + x6
                         speed   =~ x7 + x8 + x9 "

# The CRAN binary of lavaan.mi 0.1.1 fails in fitMeasures() with lavaan >= 0.7,
# because the generic's arguments were renamed. Skip when lavaan.mi itself fails.
skip_if_lavaan_mi_broken <- function(model) {
  withr::local_language("en")
  broken <- tryCatch(
    {
      lavaan::fitMeasures(model)
      FALSE
    },
    error = function(e) {
      if (!grepl("could not find symbol", conditionMessage(e), fixed = TRUE)) {
        stop(e)
      }
      TRUE
    }
  )
  if (broken) {
    skip("lavaan.mi's fitMeasures() method does not work with the installed lavaan")
  }
}

test_that("model_performance.lavaan.mi", {
  skip_if_not_installed("lavaan.mi")
  # lavaan.mi warns when it is not attached
  withr::local_package("lavaan.mi")
  data(HS20imps, package = "lavaan.mi")
  model <- lavaan.mi::cfa.mi(lavaan_mi_structure, data = HS20imps)
  skip_if_lavaan_mi_broken(model)

  expect_no_warning({
    out <- model_performance(model)
  })
  expect_named(out, unname(.lavaan_fit_map()))
  expect_identical(out$Chi2, unname(lavaan::fitMeasures(model)[["chisq"]]))
  expect_identical(
    model_performance(model, pool.method = "D2")$Chi2,
    unname(lavaan::fitMeasures(model, pool.method = "D2")[["chisq"]])
  )

  partial <- c(HS20imps[1:3], list(HS20imps[[1]][1:10, ]))
  model <- suppressWarnings(lavaan.mi::cfa.mi(lavaan_mi_structure, data = partial))
  converged <- vapply(model@convergence, function(i) i[["converged"]], logical(1))
  skip_if(all(converged), "all imputations of the partial fixture converged")
  out <- model_performance(model)
  expect_identical(out$Chi2, unname(lavaan::fitMeasures(model)[["chisq"]]))
})

test_that("model_performance.lavaan.mi, robust estimator", {
  skip_if_not_installed("lavaan.mi")
  withr::local_package("lavaan.mi")
  data(HS20imps, package = "lavaan.mi")
  model <- lavaan.mi::cfa.mi(lavaan_mi_structure, data = HS20imps[1:5], estimator = "MLM")
  skip_if_lavaan_mi_broken(model)

  expect_no_warning({
    out <- model_performance(model)
  })
  measures <- lavaan::fitMeasures(model)
  expect_named(out, c(unname(.lavaan_fit_map()), unname(.lavaan_robust_fit_map())))
  expect_identical(out$Chi2_scaled, unname(measures[["chisq.scaled"]]))
  expect_identical(out$CFI_robust, unname(measures[["cfi.robust"]]))
  expect_identical(out$RMSEA_robust, unname(measures[["rmsea.robust"]]))
})

test_that("model_performance.lavaan.mi, no imputation converged", {
  skip_if_not_installed("lavaan.mi")
  withr::local_package("lavaan.mi")
  data(HS20imps, package = "lavaan.mi")
  small <- lapply(HS20imps[1:3], function(d) d[1:10, ])
  model <- suppressWarnings(lavaan.mi::cfa.mi(lavaan_mi_structure, data = small))
  expect_warning(
    {
      out <- model_performance(model)
    },
    regex = "This lavaan.mi model did not converge"
  )
  expect_true(all(is.na(unlist(out))))
})
