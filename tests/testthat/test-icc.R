.runThisTest <- Sys.getenv("RunAllinsightTests") == "yes"

if (.runThisTest && Sys.getenv("USER") != "travis") {
  if (require("testthat") && require("performance") && require("lme4") && require("insight")) {
    context("icc")

    data(iris)
    m0 <- lm(Sepal.Length ~ Petal.Length, data = iris)
    m1 <- lmer(Sepal.Length ~ Petal.Length + (1 | Species), data = iris)

    test_that("icc", {
      expect_error(icc(m0))
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
      expect_equal(
        icc(m3),
        structure(
          list(
            ICC_decomposed = 0.388408542148012,
            ICC_CI = c(`97.5%` = -0.544516876682987,
                       `2.5%` = 0.775857482706126)
          ),
          class = "icc_decomposed",
          var_rand_intercept = 22.8685704053387,
          var_residual = 14.25791154994,
          var_total = 37.5757907428253,
          ci.var_rand_intercept = structure(
            list(
              CI = 95,
              CI_low = 8.57462826731456,
              CI_high = 57.4620121695706
            ),
            row.names = c(NA,
                          -1L),
            class = "data.frame"
          ),
          ci.var_residual = structure(
            list(
              CI = 95,
              CI_low = -18.4313656698792,
              CI_high = 35.850113863994
            ),
            row.names = c(NA,
                          -1L),
            class = "data.frame"
          ),
          ci.var_total = structure(
            list(
              CI = 95,
              CI_low = 25.1969325228524,
              CI_high = 57.5279317022377
            ),
            row.names = c(NA,
                          -1L),
            class = "data.frame"
          ),
          ci = 0.95,
          ranef = "cyl"
        ),
        tolerance = 1e-3
      )
    })
  }
}