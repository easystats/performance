if (require("testthat") && require("performance") && require("lme4")) {
  context("r2_nakagawa")

  data(iris)
  model <- lme4::lmer(Sepal.Length ~ Petal.Length + (1 | Species), data = iris)

  test_that("r2_nakagawa", {
    expect_equal(
      r2_nakagawa(model),
      structure(list(R2_conditional = 0.969409477972726, R2_marginal = 0.65846169440315), class = "r2_nakagawa"),
      tolerance = 1e-3
    )
  })

}

