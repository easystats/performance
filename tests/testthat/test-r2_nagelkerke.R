if (requiet("testthat") && requiet("performance") && requiet("MASS")) {
  test_that("r2_nagelkerke", {
    model <- glm(vs ~ wt + mpg, data = mtcars, family = "binomial")
    expect_equal(r2_nagelkerke(model), c(`Nagelkerke's R2` = 0.589959301837163), tolerance = 1e-3)
    expect_equal(r2(model), list(R2_Tjur = c(`Tjur's R2` = 0.477692621360749)), tolerance = 1e-3, ignore_attr = TRUE)
  })

  options(contrasts = c("contr.treatment", "contr.poly"))
  data(housing, package = "MASS")
  model <- polr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)

  test_that("r2_nagelkerke", {
    expect_equal(r2_nagelkerke(model), c(`Nagelkerke's R2` = 0.1084083), tolerance = 1e-3)
  })
}
