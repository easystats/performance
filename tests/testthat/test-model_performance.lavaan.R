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
  expect_equal(out$RMSEA_CI_low, unname(expected[["rmsea.ci.lower"]]))
  # a narrower interval than the default 90% interval
  expect_gt(out$RMSEA_CI_low, model_performance(model)$RMSEA_CI_low)

  # "estimator" (passed by compare_performance()) is dropped
  expect_identical(model_performance(model, estimator = "ML"), model_performance(model))
})

test_that("model_performance.lavaan, fit measures without likelihood", {
  data(HolzingerSwineford1939, package = "lavaan")
  structure <- " visual  =~ x1 + x2 + x3
                textual =~ x4 + x5 + x6
                speed   =~ x7 + x8 + x9 "
  # ordinal indicators, estimated with WLSMV, which gives no loglikelihood
  ordinal <- as.data.frame(lapply(
    HolzingerSwineford1939[paste0("x", 1:9)],
    function(x) cut(x, 3, labels = FALSE)
  ))
  model <- lavaan::cfa(structure, data = ordinal, ordered = TRUE)
  out <- model_performance(model)
  expect_named(out, unname(.lavaan_fit_map()))
  expect_identical(out$Loglikelihood, NA_real_)
  expect_identical(out$AIC, NA_real_)
  expect_identical(out$BIC, NA_real_)
  expect_identical(out$BIC_adjusted, NA_real_)
  expect_equal(out$CFI, unname(lavaan::fitmeasures(model)[["cfi"]]))
})


# entries of lavaan::fitMeasures() and the columns they map to
lavaan_fit_map <- c(
  chisq = "Chi2",
  df = "Chi2_df",
  pvalue = "p_Chi2",
  baseline.chisq = "Baseline",
  baseline.df = "Baseline_df",
  baseline.pvalue = "p_Baseline",
  gfi = "GFI",
  agfi = "AGFI",
  nfi = "NFI",
  tli = "NNFI",
  cfi = "CFI",
  rmsea = "RMSEA",
  rmsea.ci.lower = "RMSEA_CI_low",
  rmsea.ci.upper = "RMSEA_CI_high",
  rmsea.pvalue = "p_RMSEA",
  rmr = "RMR",
  srmr = "SRMR",
  rfi = "RFI",
  pnfi = "PNFI",
  ifi = "IFI",
  rni = "RNI",
  logl = "Loglikelihood",
  aic = "AIC",
  bic = "BIC",
  bic2 = "BIC_adjusted"
)

# the value a column should take: AGFI reads "agfi", or "agfi_lisrel" when
# "agfi" is absent (lavaan >= 0.7 returns only "agfi_lisrel")
expected_fit_value <- function(measures, entry) {
  if (entry == "agfi" && !"agfi" %in% names(measures)) {
    entry <- "agfi_lisrel"
  }
  if (entry %in% names(measures)) unname(measures[[entry]]) else NA_real_
}

test_that("lavaan fit-index map returns NA for missing entries", {
  data(HolzingerSwineford1939, package = "lavaan")
  structure <- " visual  =~ x1 + x2 + x3
                textual =~ x4 + x5 + x6
                speed   =~ x7 + x8 + x9 "
  model <- lavaan::cfa(structure, data = HolzingerSwineford1939)
  measures <- unclass(lavaan::fitmeasures(model))
  out <- .lavaan_fit_indices(measures)
  expect_named(out, unname(lavaan_fit_map))
  for (entry in names(lavaan_fit_map)) {
    expect_equal(
      out[[lavaan_fit_map[[entry]]]],
      expected_fit_value(measures, entry),
      info = entry
    )
  }
  expect_false(is.na(out$AGFI))
  # known value of agfi_lisrel for this model (lavaan 0.7.2)
  expect_equal(out$AGFI, 0.8937, tolerance = 1e-4)

  # each entry removed alone, for AGFI both "agfi" and "agfi_lisrel"
  for (entry in names(lavaan_fit_map)) {
    drop <- if (entry == "agfi") c("agfi", "agfi_lisrel") else entry
    out <- .lavaan_fit_indices(measures[!names(measures) %in% drop])
    expect_named(out, unname(lavaan_fit_map))
    expect_identical(out[[lavaan_fit_map[[entry]]]], NA_real_, info = entry)
    for (other in setdiff(names(lavaan_fit_map), entry)) {
      expect_equal(
        out[[lavaan_fit_map[[other]]]],
        expected_fit_value(measures, other),
        info = paste(entry, other)
      )
    }
  }

  # several entries removed
  drop <- c("gfi", "agfi", "agfi_lisrel", "rmr")
  out <- .lavaan_fit_indices(measures[!names(measures) %in% drop])
  expect_identical(out$GFI, NA_real_)
  expect_identical(out$AGFI, NA_real_)
  expect_identical(out$RMR, NA_real_)
  for (other in setdiff(names(lavaan_fit_map), c("gfi", "agfi", "rmr"))) {
    expect_equal(
      out[[lavaan_fit_map[[other]]]],
      expected_fit_value(measures, other),
      info = other
    )
  }

  # "agfi" wins over "agfi_lisrel", wherever it is in the output
  out <- .lavaan_fit_indices(c(measures, agfi = 0.123))
  expect_identical(out$AGFI, 0.123)
  out <- .lavaan_fit_indices(c(agfi = 0.123, measures))
  expect_identical(out$AGFI, 0.123)
  out <- .lavaan_fit_indices(c(measures[names(measures) != "agfi_lisrel"], agfi = 0.456))
  expect_identical(out$AGFI, 0.456)
})


# lavaan.mi ------------------------------------------------------------------

lavaan_mi_structure <- " visual  =~ x1 + x2 + x3
                         textual =~ x4 + x5 + x6
                         speed   =~ x7 + x8 + x9 "

# The CRAN binary of lavaan.mi 0.1.1 fails in fitMeasures() with lavaan >= 0.7,
# because the generic's arguments were renamed. Skip when lavaan.mi itself fails.
skip_if_lavaan_mi_broken <- function(model) {
  broken <- tryCatch(
    {
      lavaan::fitMeasures(model)
      FALSE
    },
    error = function(e) {
      # only the renamed-argument mismatch skips, any other error fails
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

  # same columns as for lavaan models, no warning
  expect_no_warning({
    out <- model_performance(model)
  })
  expect_s3_class(out, "performance_lavaan")
  expect_identical(nrow(out), 1L)
  expect_named(out, unname(lavaan_fit_map))

  # values match the pooled fit measures, default pooling and D2
  for (pool in list(list(), list(pool.method = "D2"))) {
    fm <- do.call(lavaan::fitMeasures, c(list(model), pool))
    out <- do.call(model_performance, c(list(model), pool))
    for (entry in names(lavaan_fit_map)) {
      expect_equal(
        out[[lavaan_fit_map[[entry]]]],
        expected_fit_value(unclass(fm), entry),
        info = entry
      )
    }
    # pooled output has "agfi_lisrel" but no "agfi"
    expect_equal(out$AGFI, unname(fm[["agfi_lisrel"]]))
  }
  # D2 pooling gives a different statistic than the default
  expect_false(isTRUE(all.equal(
    model_performance(model)$Chi2,
    model_performance(model, pool.method = "D2")$Chi2
  )))

  # metrics selects columns, in the requested order
  out <- model_performance(model, metrics = c("RMSEA", "Chi2"))
  expect_named(out, c("RMSEA", "Chi2"))

  # "estimator" (passed by compare_performance()) is dropped
  expect_identical(model_performance(model, estimator = "ML"), model_performance(model))

  # some imputations did not converge: lavaan.mi pools the converged ones
  partial <- c(HS20imps[1:3], list(HS20imps[[1]][1:10, ]))
  model <- suppressWarnings(lavaan.mi::cfa.mi(lavaan_mi_structure, data = partial))
  converged <- vapply(model@convergence, function(i) i[["converged"]], logical(1))
  expect_true(any(converged))
  expect_false(all(converged))
  out <- model_performance(model)
  expect_equal(out$Chi2, unname(lavaan::fitMeasures(model)[["chisq"]]))
  expect_false(is.na(out$Chi2))
})

test_that("model_performance.lavaan.mi, no imputation converged", {
  skip_if_not_installed("lavaan.mi")
  withr::local_package("lavaan.mi")
  data(HS20imps, package = "lavaan.mi")
  small <- lapply(HS20imps[1:3], function(d) d[1:10, ])
  model <- suppressWarnings(lavaan.mi::cfa.mi(lavaan_mi_structure, data = small))
  # precondition: the convergence record shows no converged imputation
  expect_false(any(vapply(model@convergence, function(i) i[["converged"]], logical(1))))

  expect_warning(
    {
      out <- model_performance(model)
    },
    regex = "This lavaan.mi model did not converge"
  )
  expect_named(out, unname(lavaan_fit_map))
  expect_true(all(is.na(unlist(out))))

  out <- expect_silent(model_performance(
    model,
    metrics = c("CFI", "SRMR"),
    verbose = FALSE
  ))
  expect_named(out, c("CFI", "SRMR"))
  expect_true(all(is.na(unlist(out))))
})
