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

skip_if_not_installed("withr")

withr::with_environment(
  new.env(),
  {
    test_that("r2_mcfadden, glmmTMB-beta-binomial", {
      skip_if_not_installed("glmmTMB")
      set.seed(101)
      dd <- data.frame(x = rnorm(200))
      dd$y <- glmmTMB::simulate_new(
        ~ 1 + x,
        newdata = dd,
        newparams = list(beta = c(0, 1), betadisp = -1),
        weights = rep(10, nrow(dd)),
        family = glmmTMB::betabinomial()
      )[[1]]
      dd$success <- round(runif(nrow(dd), 0, dd$y))
      d <<- dd

      m <- glmmTMB::glmmTMB(
        y / 10 ~ 1 + x,
        data = d,
        weights = rep(10, nrow(d)),
        family = glmmTMB::betabinomial()
      )
      out1 <- r2(m)
      out2 <- r2_mcfadden(m)
      expect_equal(out1$R2, out2$R2, tolerance = 1e-4, ignore_attr = TRUE)
      expect_equal(out1$R2, 0.06892733, tolerance = 1e-4, ignore_attr = TRUE)

      m <- glmmTMB::glmmTMB(
        cbind(y, success) ~ 1 + x,
        data = d,
        weights = rep(10, nrow(d)),
        family = glmmTMB::betabinomial()
      )
      expect_warning(r2(m), regex = "calculate accurate")
      expect_warning(r2_mcfadden(m), regex = "calculate accurate")
    })
  }
)
