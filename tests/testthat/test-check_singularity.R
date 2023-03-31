test_that("check_singularity", {
  skip_on_cran()
  skip_if_not_installed("lme4")
  data(sleepstudy, package = "lme4")
  set.seed(123)
  sleepstudy$mygrp <- sample(1:5, size = 180, replace = TRUE)
  sleepstudy$mysubgrp <- NA
  for (i in 1:5) {
    filter_group <- sleepstudy$mygrp == i
    sleepstudy$mysubgrp[filter_group] <-
      sample(1:30, size = sum(filter_group), replace = TRUE)
  }

  model <- suppressMessages(lme4::lmer(
    Reaction ~ Days + (1 | mygrp / mysubgrp) + (1 | Subject),
    data = sleepstudy
  ))
  expect_true(check_singularity(model))
})
