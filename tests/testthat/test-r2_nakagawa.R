.runThisTest <- Sys.getenv("RunAllperformanceTests") == "yes"

if (requiet("testthat") && requiet("performance") && requiet("lme4")) {
  data(iris)
  model <- lme4::lmer(Sepal.Length ~ Petal.Length + (1 | Species), data = iris)

  test_that("r2_nakagawa", {
    expect_equal(
      r2_nakagawa(model),
      structure(
        list(
          R2_conditional = c(`Conditional R2` = 0.969409477972726),
          R2_marginal = c(`Marginal R2` = 0.65846169440315)
        ),
        class = "r2_nakagawa"
      ),
      tolerance = 1e-3
    )
  })

  if (.runThisTest) {
    dat <- structure(list(
      y = structure(
        c(
          1L, 1L, 2L, 2L, 1L, 1L, 2L, 2L, 1L, 2L, 1L, 2L, 2L, 1L, 2L, 1L, 1L, 2L, 2L, 1L, 2L, 2L, 2L, 1L,
          2L, 2L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 1L, 2L, 1L, 2L, 2L, 2L, 1L, 2L, 2L, 2L, 1L, 1L, 1L, 1L,
          1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 1L, 2L, 1L, 1L, 1L, 2L, 2L, 1L, 2L, 1L, 2L,
          2L, 1L, 1L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 1L, 1L, 1L, 1L
        ),
        .Label = c("0", "1"), class = "factor"
      ),
      x1 = structure(
        c(
          2L, 1L, 2L, 2L, 2L, 2L, 2L, 1L, 2L, 1L, 1L, 1L, 2L, 1L, 2L, 2L, 2L, 2L, 1L, 2L, 2L, 2L, 2L, 2L, 2L,
          1L, 2L, 1L, 2L, 1L, 1L, 2L, 2L, 2L, 1L, 2L, 2L, 2L, 1L, 1L, 2L, 2L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 1L, 2L, 1L, 2L, 1L, 1L,
          1L, 2L, 2L, 1L, 1L, 1L, 2L, 1L, 2L, 2L, 2L, 1L, 2L, 2L, 1L, 2L, 2L, 2L, 1L, 1L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 1L, 2L, 1L,
          2L, 2L, 2L, 2L, 1L, 2L, 2L, 2L, 2L, 2L, 1L, 2L, 2L, 1L, 2L, 2L, 2L, 1L, 2L, 2L, 1L, 1L, 1L, 2L, 2L
        ),
        .Label = c("1", "2"), class = "factor"
      ),
      a1 = structure(
        c(
          9L, 1L, 1L, 5L, 9L, 7L, 1L, 1L, 9L, 5L, 5L, 5L, 5L, 6L, 4L, 3L, 5L, 9L, 9L, 6L, 1L, 9L, 9L, 9L, 9L, 5L, 2L, 7L, 7L, 7L, 4L,
          5L, 7L, 4L, 2L, 9L, 2L, 2L, 9L, 9L, 5L, 5L, 7L, 5L, 9L, 8L, 8L, 8L, 8L, 3L, 9L, 5L, 6L, 6L, 3L, 9L, 6L, 9L, 6L, 3L, 5L,
          6L, 6L, 5L, 7L, 4L, 7L, 2L, 5L, 2L, 5L, 5L, 9L, 5L, 9L, 4L, 3L, 5L, 5L, 3L, 9L, 3L, 3L, 3L, 9L, 9L, 3L, 9L, 3L, 3L, 9L,
          8L, 3L, 9L, 7L, 9L, 9L, 6L, 2L, 9L, 9L, 4L, 9L, 7L, 5L, 5L, 5L, 5L, 5L, 5L
        ),
        .Label = c("1", "2", "3", "4", "5", "6", "7", "8", "9"), class = "factor"
      ),
      a2 = structure(
        c(
          6L, 6L, 12L, 12L, 11L, 11L, 11L, 14L, 14L, 14L, 14L, 14L, 13L, 1L, 1L, 13L,
          1L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 12L, 12L, 12L, 7L, 9L, 9L, 9L, 9L, 9L, 9L, 9L, 5L, 5L, 11L, 7L, 8L, 1L,
          1L, 12L, 4L, 11L, 11L, 11L, 5L, 5L, 5L, 2L, 13L, 2L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 4L, 4L, 4L, 5L, 5L, 7L, 7L, 8L, 8L,
          8L, 9L, 9L, 9L, 7L, 7L, 8L, 8L, 9L, 10L, 10L, 10L, 10L, 1L, 10L, 1L, 10L, 13L, 13L, 6L, 11L, 11L, 14L, 2L, 2L, 2L, 2L,
          6L, 6L, 6L, 3L, 3L, 3L, 13L, 13L, 13L, 6L, 6L, 6L, 4L
        ),
        .Label = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14"), class = "factor"
      )
    ), row.names = c(NA, -110L), class = "data.frame")

    model <- glmer(y ~ x1 + (1 | a1) + (1 | a2), family = binomial(link = "logit"), data = dat)

    test_that("r2_nakagawa, by_group", {
      out <- r2_nakagawa(model, by_group = TRUE)
      expect_equal(out$R2, c(0, 0.05771, 0.07863), tolerance = 1e-4)
      expect_equal(out$Level, c("Level 1", "a1", "a2"))
    })

    test_that("icc, by_group", {
      out <- icc(model, by_group = TRUE)
      expect_equal(out$ICC, c(0.3200625, 0.1096797), tolerance = 1e-4)
      expect_equal(out$Group, c("a2", "a1"))
    })
  }
}
