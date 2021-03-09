if (require("testthat") && require("performance") && require("ISLR") && require("bayestestR")) {
  data(smarket, package = "ISLR")

  m1 <- glm(am ~ vs + wt, family = binomial(), data = mtcars)
  m2 <- glm(Direction ~ Lag1 + Volume, family = binomial(), data = Smarket)

  roc1 <- performance_roc(m1)
  roc2 <- performance_roc(m2)

  auc1 <- area_under_curve(roc1$Specificity, roc1$Sensitivity)
  auc2 <- area_under_curve(roc2$Specificity, roc2$Sensitivity)

  test_that("roc", {
    expect_equal(head(roc1$Sensitivity), c(0, 0.07692, 0.15385, 0.23077, 0.30769, 0.38462), tolerance = 1e-2)
    expect_equal(head(roc2$Sensitivity), c(0, 0, 0, 0, 0.00154, 0.00154), tolerance = 1e-2)
  })

  test_that("auc", {
    expect_equal(auc1, 0.964, tolerance = 1e-2)
    expect_equal(auc2, 0.535, tolerance = 1e-2)
  })
}
