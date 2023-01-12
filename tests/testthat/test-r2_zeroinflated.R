if (requiet("pscl")) {
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
          R2 = c(R2 = 0.179754784762554),
          R2_adjusted = c(`adjusted R2` = 0.175242984898761)
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
          R2 = c(R2 = 0.179754784762554),
          R2_adjusted = c(`adjusted R2` = 0.175242984898761)
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
          R2 = c(R2 = 0.0920246818082126),
          R2_adjusted = c(`adjusted R2` = 0.087030318121789)
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
          R2 = c(R2 = 0.0920246818082126),
          R2_adjusted = c(`adjusted R2` = 0.087030318121789)
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
