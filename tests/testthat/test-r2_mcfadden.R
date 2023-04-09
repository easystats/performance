test_that("r2_mcfadden", {
  skip_if_not_installed("MASS")
  withr::with_options(
    list(contrasts = c("contr.treatment", "contr.poly")),
    {
      data(housing, package = "MASS")
      model <- MASS::polr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)
      expect_equal(
        r2_mcfadden(model),
        structure(
          list(
            R2 = c(`McFadden's R2` = 0.0465152150591893),
            R2_adjusted = c(`adjusted McFadden's R2` = 0.0459671013089695)
          ),
          model_type = "Generalized Linear", class = "r2_generic"
        ),
        tolerance = 1e-3
      )

      expect_equal(
        r2(model),
        list(R2_Nagelkerke = c(`Nagelkerke's R2` = 0.108408289848161)),
        tolerance = 1e-3,
        ignore_attr = TRUE
      )
    }
  )
})
