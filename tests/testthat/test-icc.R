osx <- tryCatch({
  si <- Sys.info()
  if (!is.null(si["sysname"])) {
    si["sysname"] == "Darwin" || grepl("^darwin", R.version$os)
  } else {
    FALSE
  }
})


.runThisTest <- Sys.getenv("RunAllperformanceTests") == "yes"

if (.runThisTest && !osx) {
  if (requiet("brms") && requiet("rstanarm") && requiet("lme4") && requiet("nlme") && requiet("httr")) {
    skip_if_offline()

    data(iris)
    m0 <- lm(Sepal.Length ~ Petal.Length, data = iris)
    m1 <- lmer(Sepal.Length ~ Petal.Length + (1 | Species), data = iris)

    test_that("icc", {
      expect_warning(expect_null(icc(m0)))
    })

    test_that("icc", {
      expect_equal(
        icc(m1),
        data.frame(
          ICC_adjusted = 0.910433109183341, ICC_conditional = 0.310947768161385,
          ICC_unadjusted = 0.310947768161385
        ),
        tolerance = 1e-3,
        ignore_attr = TRUE
      )
    })


    # bootstrapped CIs ------------
    m <- lmer(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)
    set.seed(123)
    out <- icc(m, ci = 0.95)
    test_that("icc, CI", {
      expect_equal(out$ICC_adjusted, c(0.72166, 0.52239, 0.84024), tolerance = 1e-3)
      expect_equal(out$ICC_unadjusted, c(0.52057, 0.32429, 0.67123), tolerance = 1e-3)
    })


    m2 <- insight::download_model("stanreg_lmerMod_1")

    test_that("icc", {
      expect_equal(
        icc(m2),
        data.frame(
          ICC_adjusted = 0.399303562702568, ICC_conditional = 0.216907586891627,
          ICC_unadjusted = 0.216907586891627
        ),
        tolerance = 1e-3,
        ignore_attr = TRUE
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
        data.frame(
          ICC_adjusted = 0.930217931275196, ICC_conditional = 0.771475122370036,
          ICC_unadjusted = 0.771475122370036
        ),
        tolerance = 0.05,
        ignore_attr = TRUE
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
        structure(list(Group = c("Subject", "grp"), ICC = c(0.5896587, 0.0016551)), class = c("icc_by_group", "data.frame"), row.names = c(NA, -2L)),
        tolerance = 0.05
      )
    })

    data(iris)
    m <- nlme::lme(Sepal.Length ~ Petal.Length, random = ~ 1 | Species, data = iris)
    out <- icc(m)
    test_that("icc", {
      expect_equal(out$ICC_adjusted, 0.9104331, tolerance = 0.01)
      expect_equal(out$ICC_unadjusted, 0.3109478, tolerance = 0.01)
    })
  }
}
