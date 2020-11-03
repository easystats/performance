.runThisTest <- Sys.getenv("RunAllperformanceTests") == "yes"

if (.runThisTest) {
  if (require("testthat") && require("performance") && require("lme4") && require("nlme") && require("insight")) {
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

    data(sleepstudy)
    set.seed(12345)
    sleepstudy$grp <- sample(1:5, size = 180, replace = TRUE)
    sleepstudy$subgrp <- NA
    for (i in 1:5) {
      filter_group <- sleepstudy$grp == i
      sleepstudy$subgrp[filter_group] <-
        sample(1:30, size = sum(filter_group), replace = TRUE)
    }
    model <- lmer(
      Reaction ~ Days + (1 | grp) + (1 | Subject),
      data = sleepstudy
    )

    test_that("icc", {
      expect_equal(
        icc(model, by_group = TRUE),
        structure(list(Group = c("Subject", "grp"), ICC = c(0.5896587,  0.0016551)), class = c("icc_by_group", "data.frame"), row.names = c(NA, -2L)),
        tolerance = 0.05
      )
    })

    data(iris)
    m <- nlme::lme(Sepal.Length ~ Petal.Length, random = ~1 | Species, data = iris)
    out <- icc(m)
    test_that("icc", {
      expect_equal(out$ICC_adjusted, 0.9104331, tolerance = 0.01)
      expect_equal(out$ICC_conditional, 0.3109478, tolerance = 0.01)
    })
  }
}
