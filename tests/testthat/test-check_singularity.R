if (require("testthat") && require("performance") && require("lme4")) {
  context("check_singularity")

  data(sleepstudy)
  set.seed(1)
  sleepstudy$mygrp <- sample(1:5, size = 180, replace = TRUE)
  sleepstudy$mysubgrp <- NA
  for (i in 1:5) {
    filter_group <- sleepstudy$mygrp == i
    sleepstudy$mysubgrp[filter_group] <-
      sample(1:30, size = sum(filter_group), replace = TRUE)
  }

  expect_message(
    model <- lmer(
      Reaction ~ Days + (1 | mygrp / mysubgrp) + (1 | Subject),
      data = sleepstudy
    )
  )

  test_that("check_singularity", {
    expect_true(check_singularity(model))
  })
}