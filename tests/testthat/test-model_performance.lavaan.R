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

test_that("lavaan fit-index map returns NA for missing entries", {
  measures <- stats::setNames(seq_along(lavaan_fit_map) / 100, names(lavaan_fit_map))
  out <- .lavaan_fit_indices(measures)
  expect_named(out, unname(lavaan_fit_map))
  expect_equal(unlist(out[1, ], use.names = FALSE), unname(measures), ignore_attr = TRUE)

  # each entry removed alone
  for (entry in names(lavaan_fit_map)) {
    out <- .lavaan_fit_indices(measures[names(measures) != entry])
    expect_named(out, unname(lavaan_fit_map))
    expect_identical(out[[lavaan_fit_map[[entry]]]], NA_real_, info = entry)
    others <- setdiff(names(lavaan_fit_map), entry)
    expect_equal(
      unlist(out[1, lavaan_fit_map[others]], use.names = FALSE),
      unname(measures[others]),
      info = entry
    )
  }

  # several entries removed
  out <- .lavaan_fit_indices(measures[!names(measures) %in% c("gfi", "agfi", "rmr")])
  expect_named(out, unname(lavaan_fit_map))
  expect_identical(out$GFI, NA_real_)
  expect_identical(out$AGFI, NA_real_)
  expect_identical(out$RMR, NA_real_)
  expect_equal(out$CFI, measures[["cfi"]])
})


# lavaan.mi ------------------------------------------------------------------

lavaan_mi_structure <- " visual  =~ x1 + x2 + x3
                         textual =~ x4 + x5 + x6
                         speed   =~ x7 + x8 + x9 "

# The CRAN binary of lavaan.mi 0.1.1 fails in fitMeasures() with lavaan >= 0.7,
# because the generic's arguments were renamed. Skip when lavaan.mi itself fails.
skip_if_lavaan_mi_broken <- function(model) {
  ok <- tryCatch(
    {
      lavaan::fitMeasures(model)
      TRUE
    },
    error = function(e) FALSE
  )
  if (!ok) {
    skip("lavaan.mi's fitMeasures() method does not work with the installed lavaan")
  }
}

test_that("model_performance.lavaan.mi", {
  skip_if_not_installed("lavaan.mi")
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
      expected <- if (entry %in% names(fm)) unname(fm[[entry]]) else NA_real_
      expect_equal(out[[lavaan_fit_map[[entry]]]], expected, info = entry)
    }
  }
  # D2 pooling gives a different statistic than the default
  expect_false(isTRUE(all.equal(
    model_performance(model)$Chi2,
    model_performance(model, pool.method = "D2")$Chi2
  )))

  # metrics selects columns, in the requested order
  out <- model_performance(model, metrics = c("RMSEA", "Chi2"))
  expect_named(out, c("RMSEA", "Chi2"))
})

test_that("model_performance.lavaan.mi, no imputation converged", {
  skip_if_not_installed("lavaan.mi")
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

  out <- expect_silent(model_performance(model, metrics = c("CFI", "SRMR"), verbose = FALSE))
  expect_named(out, c("CFI", "SRMR"))
  expect_true(all(is.na(unlist(out))))
})
