if (require("testthat") && require("performance") && require("pscl")) {
  context("r2_zeroinflated")

  data(bioChemists)

  model <- zeroinfl(
    art ~ fem + mar + kid5 + ment | kid5 + phd,
    data = bioChemists
  )

  test_that("r2_zeroinflated", {
    expect_equal(
      r2_zeroinflated(model),
      structure(
        list(
          R2 = c(R2 = 0.30718048189065),
          R2_adjusted = c(`adjusted R2` = 0.303369593452205)
        ),
        model_type = "Zero-Inflated and Hurdle",
        class = "r2_generic"
      ),
      tolerance = 1e-4
    )
    expect_equal(
      r2(model),
      structure(
        list(
          R2 = c(R2 = 0.30718048189065),
          R2_adjusted = c(`adjusted R2` = 0.303369593452205)
        ),
        model_type = "Zero-Inflated and Hurdle",
        class = "r2_generic"
      ),
      tolerance = 1e-4
    )
    expect_equal(
      r2_zeroinflated(model, method = "correlation"),
      c(`R2 for ZI-models` = 0.0945232333944027),
      tolerance = 1e-4
    )
  })

  model <- hurdle(
    art ~ fem + mar + kid5 + ment | kid5 + phd,
    data = bioChemists
  )

  test_that("r2_zeroinflated", {
    expect_equal(
      r2_zeroinflated(model),
      structure(
        list(
          R2 = c(R2 = 0.317868232740263),
          R2_adjusted = c(`adjusted R2` = 0.314116132810341)
        ),
        model_type = "Zero-Inflated and Hurdle",
        class = "r2_generic"
      ),
      tolerance = 1e-4
    )
    expect_equal(
      r2(model),
      structure(
        list(
          R2 = c(R2 = 0.317868232740263),
          R2_adjusted = c(`adjusted R2` = 0.314116132810341)
        ),
        model_type = "Zero-Inflated and Hurdle",
        class = "r2_generic"
      ),
      tolerance = 1e-4
    )
    expect_equal(
      r2_zeroinflated(model, method = "correlation"),
      c(`R2 for ZI-models` = 0.08938686),
      tolerance = 1e-4
    )
  })
}