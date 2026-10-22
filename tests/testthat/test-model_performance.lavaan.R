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
  expect_named(out, unname(.lavaan_fit_map()))
  expect_identical(out$Loglikelihood, NA_real_)
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
