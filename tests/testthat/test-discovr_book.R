skip_on_cran()
skip_if_not_installed("discovr")
skip_if_not_installed("datawizard")

test_that("performance, chapter 10.3", {
  vids_tib <- discovr::video_games
  vids_cent_tib <- datawizard::center(vids_tib, c("vid_game", "caunts"))
  m <- lm(aggress ~ caunts * vid_game, data = vids_cent_tib)
  out <- model_performance(m)
  expect_identical(
    capture.output(out),
    c(
      "# Indices of model performance",
      "",
      "AIC    |   AICc |    BIC |    R2 | R2 (adj.) |  RMSE | Sigma",
      "------------------------------------------------------------",
      "3293.7 | 3293.8 | 3314.2 | 0.377 |     0.373 | 9.931 | 9.976"
    )
  )

  expect_message(
    {
      out <- test_wald(m)
    },
    regex = "Only one model was provided",
    fixed = TRUE
  )
  expect_identical(
    capture.output(print(out, table_width = Inf)),
    c(
      "Name       | Model |  df | df_diff |     F |      p",
      "---------------------------------------------------",
      "Null model |    lm | 441 |         |       |       ",
      "Full model |    lm | 438 |       3 | 88.46 | < .001",
      "Models were detected as nested (in terms of fixed parameters) and are compared in sequential order."
    )
  )
})
