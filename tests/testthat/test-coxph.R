if (require("testthat") && require("performance") && require("survival")) {
  context("survival")

  data("lung")
  lung <- subset(lung, subset = ph.ecog %in% 0:2)
  lung$sex <- factor(lung$sex, labels = c("male", "female"))
  lung$ph.ecog <- factor(lung$ph.ecog, labels = c("good", "ok", "limited"))

  m1 <- coxph(Surv(time, status) ~ sex + age + ph.ecog, data = lung)

  test_that("r2", {
    expect_equal(r2_nagelkerke(m1), c(`Nagelkerke's R2` = 0.1626795), tolerance = 1e-3)
    expect_equal(r2(m1), list(R2_Nagelkerke = c(`Nagelkerke's R2` = 0.1626795 )), tolerance = 1e-3)
  })

  test_that("model_performance", {
    perf <- model_performance(m1)

    expect_equal(perf$AIC, 1457.19458886438, tolerance = 1e-4)
    expect_equal(perf$BIC, 1469.5695896676, tolerance = 1e-4)
    expect_equal(perf$R2_Nagelkerke, 0.162679484267414, tolerance = 1e-4)
    expect_equal(perf$RMSE, 0.882014942836432, tolerance = 1e-4)

  })

  test_that("r2", {
    expect_error(r2_xu(m1))
  })

}