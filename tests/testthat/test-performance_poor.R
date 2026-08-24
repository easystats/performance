skip_if_not_installed("lme4")
skip_if_not_installed("datawizard")

test_that("performance_poor", {
  data(sleepstudy, package = "lme4")
  set.seed(123)
  sleepstudy$mygrp <- sample(1:5, size = 180, replace = TRUE)
  sleepstudy$high_reaction <- as.factor(datawizard::categorize(sleepstudy$Reaction))

  m <- lme4::glmer(
    high_reaction ~ Days + (1 | Subject),
    data = sleepstudy,
    family = "binomial"
  )
  out <- performance_poor(m)
  # fmt: skip
  expect_equal(
    out,
    data.frame(
      Parameter = c("(Intercept)", "Days"),
      Group = c("Subject", "Subject"),
      POOR = c(0.192778797972706, 0.426136228315004),
      stringsAsFactors = FALSE
    ),
    tolerance = 1e-4,
    ignore_attr = TRUE
  )

  m <- suppressWarnings(lme4::glmer(
    high_reaction ~ Days + (1 | mygrp) + (1 | Subject),
    data = sleepstudy,
    family = "binomial"
  ))
  out <- performance_poor(m)
  # fmt: skip
  expect_equal(
    out,
    data.frame(
      Parameter = c("(Intercept)", "Days", "(Intercept)", "Days"),
      Group = c("Subject", "Subject", "mygrp", "mygrp"),
      POOR = c(0.192779101384865, 0.426136323408472, 0, 0),
      stringsAsFactors = FALSE
    ),
    tolerance = 1e-4,
    ignore_attr = TRUE
  )

  # errors
  m <- glm(high_reaction ~ Days, data = sleepstudy, family = "binomial")
  expect_error(
    performance_poor(m),
    "The supplied model needs to be",
    fixed = TRUE
  )

  m <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
  expect_error(
    performance_poor(m),
    "The supplied model needs to be",
    fixed = TRUE
  )
})
