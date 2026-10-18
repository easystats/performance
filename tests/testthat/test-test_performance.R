test_that("test_performance - nested", {
  skip_if_not_installed("lavaan")
  skip_if_not_installed("lmtest")

  # Decreasing
  m1 <- lm(Sepal.Length ~ Petal.Width * Species, data = iris)
  m2 <- lm(Sepal.Length ~ Petal.Width + Species, data = iris)
  m3 <- lm(Sepal.Length ~ Petal.Width, data = iris)

  rez <- test_performance(m1, m2, m3)
  expect_equal(rez$Name, c("m1", "m2", "m3"), ignore_attr = TRUE)

  rez <- test_performance(list(m1, m2, m3))
  expect_equal(rez$Name, c("Model 1", "Model 2", "Model 3"), ignore_attr = TRUE)

  models <- list(m1, m2, m3)
  rez <- test_performance(models)
  expect_equal(rez$Name, c("Model 1", "Model 2", "Model 3"), ignore_attr = TRUE)

  models <- list(Interaction = m1, NoInteraction = m2, SingleTerm = m3)
  rez <- test_performance(models)
  expect_equal(
    rez$Name,
    c("Interaction", "NoInteraction", "SingleTerm"),
    ignore_attr = TRUE
  )

  # Increasing
  # TODO: Increasing order must be fixed and double checked, because the empty line should be the bottom one (?)
  # m1 <- lm(Sepal.Length ~ Petal.Width, data = iris)
  # m2 <- lm(Sepal.Length ~ Petal.Width + Species, data = iris)
  # m3 <- lm(Sepal.Length ~ Petal.Width * Species, data = iris)
  #
  # rez <- test_performance(m1, m2, m3)
})

test_that("test_performance - lavaan", {
  # fails on CRAN
  # * using R version 4.5.3 (2026-03-11 ucrt)
  # * using platform: x86_64-w64-mingw32
  skip_on_cran()
  skip_if_not_installed("lavaan")

  structure1 <- " visual  =~ x1 + x2 + x3
                  textual =~ x4 + x5 + x6
                  speed   =~ x7 + x8 + x9
                  visual ~~ textual + speed "
  m1 <- suppressWarnings(lavaan::cfa(structure1, data = lavaan::HolzingerSwineford1939))

  structure2 <- " visual  =~ x1 + x2 + x3
                  textual =~ x4 + x5 + x6
                  speed   =~ x7 + x8 + x9
                  visual ~~ 0 * textual + speed "
  m2 <- suppressWarnings(lavaan::cfa(structure2, data = lavaan::HolzingerSwineford1939))

  expect_no_warning(test_performance(m1, m2))
  rez <- test_performance(m1, m2)
  expect_s3_class(rez, "test_performance")
  expect_match(
    attributes(format(rez))$table_footer,
    "assumes that each model is nested",
    fixed = TRUE
  )
  expect_identical(rez$Name, c("m1", "m2"))
  expect_true(all(c("BF", "df", "df_diff", "Chi2", "p") %in% colnames(rez)))

  lrt <- test_likelihoodratio(m1, m2)
  expect_equal(rez$Chi2, lrt$Chi2, tolerance = 1e-3)
  expect_equal(rez$p, lrt$p, tolerance = 1e-3)

  bic <- c(stats::BIC(m1), stats::BIC(m2))
  expect_equal(rez$BF[2], exp((bic[1] - bic[2]) / 2), tolerance = 1e-3)

  # models are compared in order of their df, whatever the input order
  rez2 <- test_performance(m2, m1)
  expect_identical(rez2$Name, c("m1", "m2"))
  expect_equal(rez2$Chi2, rez$Chi2, tolerance = 1e-3)

  # models fit to different observations or observed variables
  m3 <- suppressWarnings(lavaan::cfa(
    structure1,
    data = lavaan::HolzingerSwineford1939[1:200, ]
  ))
  expect_warning(
    expect_warning(
      expect_warning(test_performance(m1, m3), regex = "same observations"),
      regex = "same degrees of freedom"
    ),
    regex = "not nested"
  )
  structure4 <- " visual  =~ x1 + x2
                  textual =~ x4 + x5 + x6
                  speed   =~ x7 + x8 + x9 "
  m4 <- suppressWarnings(lavaan::cfa(structure4, data = lavaan::HolzingerSwineford1939))
  expect_warning(
    expect_warning(test_performance(m1, m4), regex = "same observations"),
    regex = "different set of observed variables"
  )

  expect_error(test_vuong(m1, m2), regex = "lavaan models")
  expect_error(test_wald(m1, m2), regex = "lavaan models")
})

test_that("test_performance - single model", {
  m1 <- lm(Sepal.Length ~ 1, data = iris)
  m2 <- lm(Sepal.Length ~ Petal.Width, data = iris)
  expect_message(test_performance(m2), regex = "Only one model")
  expect_silent(test_performance(m2, verbose = FALSE))

  out1 <- test_performance(m2, verbose = FALSE)
  out2 <- test_performance(m1, m2, verbose = FALSE)
  expect_equal(out1$Omega2, out2$Omega2, tolerance = 1e-3)
})
