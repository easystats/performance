.runThisTest <- Sys.getenv("RunAllinsightTests") == "yes"

if (.runThisTest && Sys.getenv("USER") != "travis") {
  if (require("testthat") && require("performance") && require("lme4") && require("insight")) {
    context("icc")

    data(iris)
    m0 <- lm(Sepal.Length ~ Petal.Length, data = iris)
    m1 <- lmer(Sepal.Length ~ Petal.Length + (1 | Species), data = iris)

    test_that("icc", {
      expect_warning(expect_null(icc(m0)))
    })

    test_that("icc", {
      expect_equal(
        icc(m1),
        structure(list(ICC_adjusted = 0.910433115331481, ICC_conditional = 0.310947783569576), class = "icc"),
        tolerance = 1e-3
      )
    })

    m2 <- insight::download_model("stanreg_lmerMod_1")

    test_that("icc", {
      expect_equal(
        icc(m2),
        structure(list(ICC_adjusted = 0.399303562702568, ICC_conditional = 0.216907586891627), class = "icc"),
        tolerance = 1e-3
      )
    })

    m3 <- insight::download_model("brms_mixed_1")

    test_that("icc", {
      set.seed(123)
      expect_equal(
        icc(m3),
        structure(
          list(
            ICC_decomposed = 0.390068271523025,
            ICC_CI = c(`97.5%` = -0.5338626026371, `2.5%` = 0.783366175564956)
          ),
          class = "icc_decomposed",
          var_rand_intercept = 22.7916688806349,
          var_residual = 14.4878967615321,
          var_total = 37.662418617231,
          ci.var_rand_intercept = structure(
            list(
              CI = 95,
              CI_low = 8.44552116382023,
              CI_high = 58.0512742208554
            ),
            row.names = c(NA, -1L),
            class = c("ci", "see_ci", "data.frame")
          ),
          ci.var_residual = structure(
            list(
              CI = 95, CI_low = -18.1777017649845,
              CI_high = 35.5471388201185
            ),
            row.names = c(NA, -1L),
            class = c("ci", "see_ci", "data.frame")
          ),
          ci.var_total = structure(
            list(
              CI = 95,
              CI_low = 25.1996582781989,
              CI_high = 56.4463902022863
            ),
            row.names = c(NA, -1L),
            class = c("ci", "see_ci", "data.frame")
          ),
          ci = 0.95,
          ranef = "cyl"
        ),
        tolerance = 1e-3
      )
    })
  }
}