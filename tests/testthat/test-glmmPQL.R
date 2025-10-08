skip_if_not_installed("MASS")
test_that("r2", {
  example_dat <- data.frame(
    prop = c(0.2, 0.2, 0.5, 0.7, 0.1, 1, 1, 1, 0.1),
    size = c(
      "small",
      "small",
      "small",
      "large",
      "large",
      "large",
      "large",
      "small",
      "small"
    ),
    x = c(0.1, 0.1, 0.8, 0.7, 0.6, 0.5, 0.5, 0.1, 0.1),
    species = c("sp1", "sp1", "sp2", "sp2", "sp3", "sp3", "sp4", "sp4", "sp4"),
    stringsAsFactors = FALSE
  )
  mn <- MASS::glmmPQL(
    prop ~ x + size,
    random = ~ 1 | species,
    family = "quasibinomial",
    data = example_dat
  )
  expect_message(performance_score(mn), regex = "Can't calculate")
})
