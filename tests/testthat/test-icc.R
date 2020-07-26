.runThisTest <- Sys.getenv("RunAllperformanceTests") == "yes"

if (.runThisTest) {
  if (require("testthat") && require("performance") && require("lme4") && require("insight")) {
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
        variance_decomposition(m3)$ICC_decomposed,
        0.3262006,
        tolerance = 0.05
      )
    })

    test_that("icc", {
      set.seed(123)
      expect_equal(
        icc(m3),
        structure(list(ICC_adjusted = 0.930217931275196, ICC_conditional = 0.771475122370036), class = "icc"),
        tolerance = 0.05
      )
    })
  }
}
