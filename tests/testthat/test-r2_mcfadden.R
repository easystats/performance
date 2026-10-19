skip_if_not_installed("withr")

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
            R2_adjusted = c(`adjusted McFadden's R2` = 0.0421303069)
          ),
          model_type = "Generalized Linear",
          class = "r2_generic"
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

test_that("r2, glmmTMB negative-binomial without random effects", {
  skip_if_not_installed("glmmTMB")
  set.seed(101)
  dd <- data.frame(x = rnorm(200))
  dd$y <- rnbinom(200, mu = exp(0.5 + 1 * dd$x), size = 2)

  # nbinom2 previously errored ("does not support models of class `glmmTMB`
  # without random effects and from nbinom2-family ..."); now returns McFadden's.
  m2 <- glmmTMB::glmmTMB(y ~ 1 + x, data = dd, family = glmmTMB::nbinom2())
  out <- r2(m2)
  expect_equal(out$R2, r2_mcfadden(m2)$R2, tolerance = 1e-4, ignore_attr = TRUE)
  expect_equal(out$R2, 0.1521543, tolerance = 1e-4, ignore_attr = TRUE)

  # nbinom1 is handled by the same branch.
  m1 <- glmmTMB::glmmTMB(y ~ 1 + x, data = dd, family = glmmTMB::nbinom1())
  expect_equal(r2(m1)$R2, 0.1406573, tolerance = 1e-4, ignore_attr = TRUE)
})

test_that("adjusted McFadden's R2 penalizes by the number of parameters", {
  m <- glm(am ~ mpg + hp + wt, data = mtcars, family = binomial)
  out <- r2_mcfadden(m)

  ll_full <- as.vector(insight::get_loglikelihood(m))
  ll_null <- as.vector(insight::get_loglikelihood(insight::null_model(m)))
  k <- insight::n_parameters(m)

  expect_identical(k, 4L)
  expect_equal(
    out$R2_adjusted,
    1 - ((ll_full - k) / ll_null),
    tolerance = 1e-6,
    ignore_attr = TRUE
  )

  # reference values from DescTools::PseudoR2(m, c("McFadden", "McFaddenAdj"))
  expect_equal(out$R2, 0.7972202, tolerance = 1e-5, ignore_attr = TRUE)
  expect_equal(out$R2_adjusted, 0.6121632, tolerance = 1e-5, ignore_attr = TRUE)
})
