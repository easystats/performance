if (require("testthat") && require("performance") && require("glmmTMB")) {
  set.seed(123)
  data(Salamanders)
  m <- glm(count ~ spp + mined, family = poisson, data = Salamanders)

  test_that("check_zeroinflation", {
    expect_equal(check_zeroinflation(m),
      structure(
        list(
          predicted.zeros = 298,
          observed.zeros = 387L,
          ratio = 0.770025839793282,
          tolerance = 0.05
        ),
        class = "check_zi"
      ),
      tolerance = 1e-3
    )
  })
}
