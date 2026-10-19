skip_if_not_installed("lme4")
skip_if_not_installed("datawizard")

test_that("performance_mor", {
  data(sleepstudy, package = "lme4")
  set.seed(123)
  sleepstudy$mygrp <- sample(1:5, size = 180, replace = TRUE)
  sleepstudy$high_reaction <- as.factor(datawizard::categorize(sleepstudy$Reaction))

  m <- lme4::glmer(
    high_reaction ~ Days + (1 | Subject),
    data = sleepstudy,
    family = "binomial"
  )
  out <- performance_mor(m)
  expect_equal(
    out,
    data.frame(Group = "Subject", MOR = 14.7258491605265, stringsAsFactors = FALSE),
    tolerance = 1e-4,
    ignore_attr = TRUE
  )

  m <- suppressWarnings(lme4::glmer(
    high_reaction ~ Days + (1 | mygrp) + (1 | Subject),
    data = sleepstudy,
    family = "binomial"
  ))
  out <- performance_mor(m)
  expect_equal(
    out,
    data.frame(
      Group = c("Subject", "mygrp"),
      MOR = c(14.7258119253219, 1),
      stringsAsFactors = FALSE
    ),
    tolerance = 1e-4,
    ignore_attr = TRUE
  )

  # errors
  m <- glm(high_reaction ~ Days, data = sleepstudy, family = "binomial")
  expect_error(
    performance_mor(m),
    "The supplied model needs to be",
    fixed = TRUE
  )

  m <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
  expect_error(
    performance_mor(m),
    "The supplied model needs to be",
    fixed = TRUE
  )
})
