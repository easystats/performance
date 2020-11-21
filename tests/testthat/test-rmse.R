if (require("testthat") && require("performance") && require("lme4")) {
  data(sleepstudy, package = "lme4")

  m1.1 <- glm(Reaction ~ Days, data = sleepstudy, family = gaussian())
  m1.2 <- glm(Reaction ~ Days, data = sleepstudy, family = gaussian("log"))
  m1.3 <- glm(Reaction ~ Days, data = sleepstudy, family = gaussian("inverse"))

  m2.1 <- glm(Reaction ~ Days, data = sleepstudy, family = inverse.gaussian())
  m2.2 <- glm(Reaction ~ Days, data = sleepstudy, family = inverse.gaussian("log"))
  m2.3 <- glm(Reaction ~ Days, data = sleepstudy, family = inverse.gaussian("inverse"))

  cp <- compare_performance(m1.1, m1.2, m1.3, m2.1, m2.2, m2.3)

  test_that("rmse", {
    expect_equal(cp$RMSE, c(47.4489, 47.39881, 47.38701, 47.41375, 47.39979, 47.38705), tolerance = 1e-3)
    expect_equal(cp$BF, c(NA, 1.20939, 1.26483, 1234.82916, 1287.70981, 1360.58689), tolerance = 1e-3)
  })
}
