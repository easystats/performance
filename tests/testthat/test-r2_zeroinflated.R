if (require("testthat") && require("performance") && require("pscl")) {
  context("r2_zeroinflated")

  data(bioChemists)

  model <- zeroinfl(
    art ~ fem + mar + kid5 + ment | kid5 + phd,
    data = bioChemists
  )

  test_that("r2_zeroinflated", {
    expect_equal(r2_zeroinflated(model), 0.09452323, tolerance = 1e-4)
    expect_equal(r2(model), 0.09452323, tolerance = 1e-4)
  })

  model <- hurdle(
    art ~ fem + mar + kid5 + ment | kid5 + phd,
    data = bioChemists
  )

  test_that("r2_zeroinflated", {
    expect_equal(r2_zeroinflated(model), 0.08938686, tolerance = 1e-4)
    expect_equal(r2(model), 0.08938686, tolerance = 1e-4)
  })
}