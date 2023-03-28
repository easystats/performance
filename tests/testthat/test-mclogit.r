if (requiet("mclogit")) {
  data(Transport)
  mod_mb <- mblogit(factor(gear) ~ mpg + hp, data = mtcars, trace = FALSE)
  mod_mc <- mclogit(resp | suburb ~ distance + cost, data = Transport, trace = FALSE)

  test_that("r2 Nagelkerke", {
    expect_equal(
      r2_nagelkerke(mod_mb),
      getSummary.mblogit(mod_mb)$sumstat["Nagelkerke"],
      ignore_attr = TRUE,
      tolerance = 1e-4
    )
    expect_equal(
      r2_nagelkerke(mod_mc),
      getSummary.mclogit(mod_mc)$sumstat["Nagelkerke"],
      ignore_attr = TRUE,
      tolerance = 1e-4
    )
  })

  test_that("r2 McFadden", {
    expect_equal(
      r2_mcfadden(mod_mb),
      getSummary.mblogit(mod_mb)$sumstat["McFadden"],
      ignore_attr = TRUE,
      tolerance = 1e-4
    )
    expect_equal(
      r2_mcfadden(mod_mc),
      getSummary.mclogit(mod_mc)$sumstat["McFadden"],
      ignore_attr = TRUE,
      tolerance = 1e-4
    )
  })

  test_that("r2 CoxSnell", {
    expect_equal(
      r2_coxsnell(mod_mb),
      getSummary.mblogit(mod_mb)$sumstat["Cox.Snell"],
      ignore_attr = TRUE,
      tolerance = 1e-4
    )
    expect_equal(
      r2_coxsnell(mod_mc),
      getSummary.mclogit(mod_mc)$sumstat["Cox.Snell"],
      ignore_attr = TRUE,
      tolerance = 1e-4
    )
  })

  test_that("model_performance", {
    out <- capture.output(print(model_performance(mod_mb)))
    expect_identical(
      out,
      c(
        "# Indices of model performance",
        "",
        "AIC    |    BIC | Nagelkerke's R2 |  RMSE | Sigma",
        "-------------------------------------------------",
        "38.823 | 47.618 |           0.836 | 0.298 | 1.016"
      )
    )
    out <- capture.output(print(model_performance(mod_mc)))
    expect_identical(
      out,
      c(
        "# Indices of model performance",
        "",
        "AIC    |    BIC | Nagelkerke's R2 |  RMSE | Sigma",
        "-------------------------------------------------",
        "13.228 | 24.424 |           0.998 | 0.009 | 0.068"
      )
    )
  })
}
