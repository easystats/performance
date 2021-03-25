if (require("testthat") &&
  require("performance") &&
  require("insight") &&
  require("AER") &&
  require("ordinal") &&
  require("betareg")) {
  data("Affairs", package = "AER")
  data("GasolineYield")
  data("wine")
  data("CigarettesSW")

  CigarettesSW$rprice <- with(CigarettesSW, price / cpi)
  CigarettesSW$rincome <- with(CigarettesSW, income / population / cpi)
  CigarettesSW$tdiff <- with(CigarettesSW, (taxs - tax) / cpi)

  m1 <- betareg(yield ~ batch + temp, data = GasolineYield)
  m3 <- clm(rating ~ temp * contact, data = wine)
  m4 <- clm2(rating ~ temp * contact, data = wine)
  m5 <- ivreg(
    log(packs) ~ log(rprice) + log(rincome) | log(rincome) + tdiff + I(tax / cpi),
    data = CigarettesSW,
    subset = year == "1995"
  )

  test_that("model_performance various", {
    expect_equal(model_performance(m1, verbose = FALSE)$R2, 0.9617312, tolerance = 1e-4)
    expect_equal(model_performance(m3, verbose = FALSE)$R2_Nagelkerke, 0.4042792, tolerance = 1e-4)
    expect_equal(model_performance(m4, verbose = FALSE)$R2_Nagelkerke, 0.4042792, tolerance = 1e-4)
    expect_equal(model_performance(m5, verbose = FALSE)$R2, 0.4294224, tolerance = 1e-4)
  })
}

## DirichletReg is currently orphaned

# if (require("testthat") && require("performance") && require("DirichletReg")) {
#   set.seed(123)
#   data("ArcticLake")
#
#   ALake <- ArcticLake
#   ALake$Y <- suppressWarnings(DR_data(ALake[, 1:3]))
#
#   # fit a quadratic Dirichlet regression models ("common")
#   res1 <- DirichReg(Y ~ depth + I(depth^2), ALake)
#
#   test_that("model_performance (Dirichlet regression)", {
#     expect_equal(
#       performance::model_performance(res1),
#       structure(
#         list(
#           AIC = -199.993722776335,
#           BIC = -185.021667961168,
#           R2_Nagelkerke = 0.0405982703444639,
#           RMSE = 0.922951614921502
#         ),
#         class = c(
#           "performance_model",
#           "data.frame"
#         ),
#         row.names = 1L,
#         r2 = list(names = "R2_Nagelkerke")
#       ),
#       tolerance = 1e-3
#     )
#   })
# }
