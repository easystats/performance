test_that("check_singularity, lme4", {
  skip_on_cran()
  skip_if_not_installed("lme4")
  data(sleepstudy, package = "lme4")
  set.seed(123)
  sleepstudy$mygrp <- sample.int(5, size = 180, replace = TRUE)
  sleepstudy$mysubgrp <- NA
  for (i in 1:5) {
    filter_group <- sleepstudy$mygrp == i
    sleepstudy$mysubgrp[filter_group] <-
      sample.int(30, size = sum(filter_group), replace = TRUE)
  }

  model <- suppressMessages(lme4::lmer(
    Reaction ~ Days + (1 | mygrp / mysubgrp) + (1 | Subject),
    data = sleepstudy
  ))
  expect_true(check_singularity(model))

  # term-wise
  out <- check_singularity(model, check = "terms")
  expect_identical(
    out,
    c(`mysubgrp:mygrp` = TRUE, Subject = FALSE, mygrp = FALSE)
  )
})


test_that("check_singularity", {
  skip_on_cran()
  skip_if_not_installed("glmmTMB")
  set.seed(101)
  dd <- expand.grid(x = factor(1:6), f = factor(1:20), rep = 1:5)
  dd$y <- glmmTMB::simulate_new(~ 1 + (x | f),
    newdata = dd,
    newparam = list(
      beta = 0,
      theta = rep(0, 21),
      betadisp = 0
    )
  )[[1]]
  expect_warning(expect_warning({
    m2 <- glmmTMB::glmmTMB(y ~ 1 + (x | f), data = dd, REML = FALSE)
  }))
  expect_true(check_singularity(m2))

  data(Salamanders, package = "glmmTMB")
  m <- suppressWarnings(glmmTMB::glmmTMB(
    count ~ spp + mined + (1 | site),
    data = Salamanders[Salamanders$count > 0, , drop = FALSE],
    family = glmmTMB::truncated_nbinom2(),
    ziformula = ~ spp + (1 | site),
    dispformula = ~ spp + (1 | site)
  ))
  out <- check_singularity(m, check = "terms")
  expect_identical(
    out,
    list(
      cond = c(`1 | site` = TRUE),
      zi = c(`1 | site` = TRUE),
      disp = c(`1 | site` = TRUE)
    )
  )
})
