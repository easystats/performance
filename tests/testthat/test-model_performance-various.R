test_that("model_performance various", {
  skip_if_not_installed("AER")
  skip_if_not_installed("ordinal")
  skip_if_not_installed("betareg")

  data("Affairs", package = "AER")
  data("GasolineYield", package = "betareg")
  data("wine", package = "ordinal")
  data("CigarettesSW", package = "AER")

  CigarettesSW$rprice <- with(CigarettesSW, price / cpi)
  CigarettesSW$rincome <- with(CigarettesSW, income / population / cpi)
  CigarettesSW$tdiff <- with(CigarettesSW, (taxs - tax) / cpi)

  m1 <- betareg::betareg(yield ~ batch + temp, data = GasolineYield)
  m3 <- ordinal::clm(rating ~ temp * contact, data = wine)
  m4 <- ordinal::clm2(rating ~ temp * contact, data = wine)
  m5 <- AER::ivreg(
    log(packs) ~ log(rprice) + log(rincome) | log(rincome) + tdiff + I(tax / cpi),
    data = CigarettesSW,
    subset = year == "1995"
  )

  expect_equal(model_performance(m1, verbose = FALSE)$R2, 0.9617312, tolerance = 1e-4)
  expect_equal(model_performance(m3, verbose = FALSE)$R2_Nagelkerke, 0.4042792, tolerance = 1e-4)
  expect_equal(model_performance(m4, verbose = FALSE)$R2_Nagelkerke, 0.4042792, tolerance = 1e-4)
  expect_equal(model_performance(m5, verbose = FALSE)$R2, 0.4294224, tolerance = 1e-4)

  mp <- model_performance(m5)
  ms <- summary(m5, diagnostics = TRUE)
  expect_equal(mp$Sargan, ms$diagnostics["Sargan", 3])
  expect_equal(mp$Sargan_p, ms$diagnostics["Sargan", 4])
  expect_equal(mp$Wu_Hausman, ms$diagnostics["Wu-Hausman", 3])
  expect_equal(mp$Wu_Hausman_p, ms$diagnostics["Wu-Hausman", 4])
  expect_equal(mp$weak_instruments, ms$diagnostics["Weak instruments", 3])
  expect_equal(mp$weak_instruments_p, ms$diagnostics["Weak instruments", 4])
})


# DirichletReg is currently orphaned
test_that("model_performance (Dirichlet regression)", {
  skip_if_not_installed("DirichletReg")
  set.seed(123)
  data("ArcticLake", package = "DirichletReg")

  ALake <- ArcticLake
  ALake$Y <- suppressWarnings(DirichletReg::DR_data(ALake[, 1:3]))

  # fit a quadratic Dirichlet regression models ("common")
  res1 <- DirichletReg::DirichReg(Y ~ depth + I(depth^2), ALake)
  expect_equal(
    model_performance(res1),
    structure(
      list(
        AIC = -199.993722776335,
        BIC = -185.021667961168,
        R2_Nagelkerke = 0.0405982703444639,
        RMSE = 0.922951614921502
      ),
      class = c(
        "performance_model",
        "data.frame"
      ),
      row.names = 1L,
      r2 = list(names = "R2_Nagelkerke")
    ),
    tolerance = 1e-3
  )
})

