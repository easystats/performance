skip_on_cran()
skip_if_not_installed("psych")
skip_if_not_installed("parameters")
skip_if_not_installed("GPArotation")

test_that("model_performance.psych", {
  raq_items <- as.data.frame(discovr::raq)
  raq_items$id <- NULL

  raq_fa <- parameters::factor_analysis(
      raq_items,
      n = 4,
      scores = "tenBerge",
      cor = "poly",
      standardize = FALSE
  )

  expect_snapshot(print(raq_fa))
  expect_snapshot(print(raq_fa, threshold = 0.4))
  expect_snapshot(summary(raq_fa))

  # FA
  out <- model_performance(raq_fa)
  expect_snapshot(print(out))

  # PCA
  data(Harman.5, package = "psych")
  pc <- psych::principal(Harman.5, 2, rotate = "varimax")
  out <- model_performance(pc)
  expect_snapshot(print(out))

  # Omega
  data(mtcars)
  m <- psych::omega(mtcars, nfactors = 3, plot = FALSE)
  out <- model_performance(m)
  expect_snapshot(print(out, table_width = Inf))

  m <- item_omega(mtcars, n = 3)
  out <- model_performance(m)
  expect_snapshot(print(out, table_width = Inf))
})


test_that("check_residuals.psych", {
  data(Harman.5, package = "psych")
  pc <- psych::principal(Harman.5, 2, rotate = "varimax")
  out <- check_residuals(pc)
  expect_equal(as.vector(out), 0.9137001, tolerance = 1e-4)
})
