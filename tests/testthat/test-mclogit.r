skip_if_not_installed("mclogit")

data(Transport, package = "mclogit")
mod_mb <- mclogit::mblogit(factor(gear) ~ mpg + hp, data = mtcars, trace = FALSE)
mod_mc <- mclogit::mclogit(resp | suburb ~ distance + cost, data = Transport, trace = FALSE)

test_that("r2 Nagelkerke", {
  expect_equal(
    r2_nagelkerke(mod_mb),
    mclogit::getSummary.mblogit(mod_mb)$sumstat["Nagelkerke"],
    ignore_attr = TRUE,
    tolerance = 1e-4
  )
  expect_equal(
    r2_nagelkerke(mod_mc),
    mclogit::getSummary.mclogit(mod_mc)$sumstat["Nagelkerke"],
    ignore_attr = TRUE,
    tolerance = 1e-4
  )
})

test_that("r2 McFadden", {
  expect_equal(
    r2_mcfadden(mod_mb),
    mclogit::getSummary.mblogit(mod_mb)$sumstat["McFadden"],
    ignore_attr = TRUE,
    tolerance = 1e-4
  )
  expect_equal(
    r2_mcfadden(mod_mc),
    mclogit::getSummary.mclogit(mod_mc)$sumstat["McFadden"],
    ignore_attr = TRUE,
    tolerance = 1e-4
  )
})

test_that("r2 CoxSnell", {
  expect_equal(
    r2_coxsnell(mod_mb),
    mclogit::getSummary.mblogit(mod_mb)$sumstat["Cox.Snell"],
    ignore_attr = TRUE,
    tolerance = 1e-4
  )
  expect_equal(
    r2_coxsnell(mod_mc),
    mclogit::getSummary.mclogit(mod_mc)$sumstat["Cox.Snell"],
    ignore_attr = TRUE,
    tolerance = 1e-4
  )
})

test_that("model_performance", {
  expect_snapshot(model_performance(mod_mb))
  expect_snapshot(model_performance(mod_mc))
})

