skip_on_os("mac")
skip_on_cran()

test_that("check_predictions", {
  data(mtcars)
  model <- lm(mpg ~ disp, data = mtcars)
  set.seed(99)
  out <- check_predictions(model)

  expect_named(
    out,
    c(
      "sim_1", "sim_2", "sim_3", "sim_4", "sim_5", "sim_6", "sim_7",
      "sim_8", "sim_9", "sim_10", "sim_11", "sim_12", "sim_13", "sim_14",
      "sim_15", "sim_16", "sim_17", "sim_18", "sim_19", "sim_20", "sim_21",
      "sim_22", "sim_23", "sim_24", "sim_25", "sim_26", "sim_27", "sim_28",
      "sim_29", "sim_30", "sim_31", "sim_32", "sim_33", "sim_34", "sim_35",
      "sim_36", "sim_37", "sim_38", "sim_39", "sim_40", "sim_41", "sim_42",
      "sim_43", "sim_44", "sim_45", "sim_46", "sim_47", "sim_48", "sim_49",
      "sim_50", "y"
    )
  )
  expect_equal(
    out$sim_1,
    c(
      23.70112, 24.56502, 25.43419, 20.40954, 13.58266, 20.72532, 
      11.95366, 25.14559, 22.61286, 18.48403, 20.26737, 21.2291, 20.67149,
      10.07628, 0.25886, 10.64176, 10.18407, 20.68235, 28.10115, 27.55045,
      28.22301, 18.94021, 16.87727, 14.05421, 13.8378, 28.13797, 26.86451,
      23.90539, 10.68719, 28.17587, 21.65853, 26.07681
    ),
    tolerance = 1e-4
  )
})
