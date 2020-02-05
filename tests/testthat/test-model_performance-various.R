if (require("testthat") &&
  require("performance") &&
  require("insight") &&
  require("AER") &&
  require("ordinal") &&
  require("betareg")) {
  data("Affairs", package = "AER")

  m1 <- insight::download_model("betareg_1")
  m3 <- insight::download_model("clm_1")
  m4 <- insight::download_model("clm2_1")
  m5 <- insight::download_model("ivreg_1")

  test_that("model_performance various", {
    expect_equal(model_performance(m1)$R2, 0.9617312, tolerance = 1e-4)
    expect_equal(model_performance(m3)$R2_Nagelkerke, 0.4042792, tolerance = 1e-4)
    expect_equal(model_performance(m4)$R2_Nagelkerke, 0.4042792, tolerance = 1e-4)
    expect_equal(model_performance(m5)$R2, 0.4294224, tolerance = 1e-4)
  })
}


if (require("testthat") && require("performance") && require("DirichletReg")) {
  set.seed(123)
  library(DirichletReg)

  ALake <- ArcticLake
  ALake$Y <- suppressWarnings(DR_data(ALake[, 1:3]))

  # fit a quadratic Dirichlet regression models ("common")
  res1 <- DirichReg(Y ~ depth + I(depth^2), ALake)

  test_that("model_performance (Dirichlet regression)", {
    expect_equal(
      performance::model_performance(res1),
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
}
