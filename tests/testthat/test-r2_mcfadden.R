if (require("MASS")) {
  context("r2_mcfadden")

  options(contrasts = c("contr.treatment", "contr.poly"))
  data(housing)
  model <- polr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)

  test_that("r2_mcfadden", {
    testthat::expect_equal(r2_mcfadden(model), c(`McFadden's R2` = 0.0465152150591893), tolerance = 1e-3)
  })

}

