if (require("testthat") &&
  require("performance") &&
  require("lme4")) {
  data(mtcars)
  m <- glmer(vs ~ mpg + (1 | gear), family = "binomial", data = mtcars)

  test_that("performance_roc", {
    roc <- performance_roc(m)
    expect_equal(
      roc$Sensivity,
      c(0, 0.07143, 0.14286, 0.21429, 0.28571, 0.35714, 0.42857, 0.5,
        0.57143, 0.57143, 0.64286, 0.64286, 0.64286, 0.71429, 0.78571,
        0.85714, 0.85714, 0.92857, 0.92857, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1),
      tolerance = 1e-3
    )
  })
}
