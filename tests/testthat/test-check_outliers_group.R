skip_if_not_installed("lme4")
skip_if_not_installed("glmmTMB")

# lme4 oracle: cooks.distance() for stats::influence() output uses the factor
# (G - p) / (G * p), so multiplying by G / (G - p) gives the
# (b - b_(g))' V(b)^-1 (b - b_(g)) / p scale. The values of
# cooks.distance() are unnamed and in the order of `inf$deleted`.
.test_lme4_group_cooks <- function(model, group, data) {
  inf <- stats::influence(model, groups = group, data = data)
  n_groups <- length(inf$deleted)
  n_params <- length(lme4::fixef(model))
  stats::setNames(
    stats::cooks.distance(inf) * n_groups / (n_groups - n_params),
    as.character(inf$deleted)
  )
}

.test_group_distances <- function(model, group) {
  out <- as.data.frame(check_outliers(model, group = group))
  stats::setNames(out$Distance_Cook, as.character(out$Group))
}


test_that("check_outliers, group Cook's distance for lmer equals stats::influence()", {
  data(sleepstudy, package = "lme4")
  set.seed(42)
  d <- sleepstudy[-sample(nrow(sleepstudy), 40), ]
  models <- list(
    balanced = lme4::lmer(Reaction ~ Days + (Days | Subject), sleepstudy),
    unbalanced = lme4::lmer(Reaction ~ Days + (Days | Subject), d),
    scaled = lme4::lmer(Reaction ~ scale(Days) + (1 | Subject), d)
  )
  model_data <- list(balanced = sleepstudy, unbalanced = d, scaled = d)
  for (name in names(models)) {
    expected <- .test_lme4_group_cooks(models[[name]], "Subject", model_data[[name]])
    actual <- .test_group_distances(models[[name]], "Subject")
    expect_length(actual, 18)
    expect_named(actual, names(expected), ignore.order = TRUE)
    expect_equal(actual[names(expected)], expected, tolerance = 1e-6, info = name)
  }
})


test_that("check_outliers, group Cook's distance for glmmTMB equals lmer", {
  # glmmTMB and lme4 give the same REML estimates and covariance matrices on
  # this balanced design. On unbalanced data the REML covariance matrices
  # differ by about one percent, so the tolerance only holds for balanced data.
  data(sleepstudy, package = "lme4")
  m_lmer <- lme4::lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
  m_tmb <- glmmTMB::glmmTMB(
    Reaction ~ Days + (Days | Subject),
    data = sleepstudy,
    REML = TRUE
  )
  expected <- .test_group_distances(m_lmer, "Subject")
  actual <- .test_group_distances(m_tmb, "Subject")
  expect_named(actual, names(expected))
  expect_equal(actual, expected, tolerance = 1e-4)
})


test_that("check_outliers, group Cook's distance for glmer and glmmTMB", {
  data(cbpp, package = "lme4")
  m_glmer <- lme4::glmer(
    cbind(incidence, size - incidence) ~ period + (1 | herd),
    family = binomial,
    data = cbpp
  )
  expected <- .test_lme4_group_cooks(m_glmer, "herd", cbpp)
  actual <- .test_group_distances(m_glmer, "herd")
  expect_length(actual, 15)
  expect_equal(actual[names(expected)], expected, tolerance = 1e-6)

  # glmmTMB: test-local loop that refits without the rows of each level,
  # starting at the full-model estimates, then
  # (b - b_(g))' V(b)^-1 (b - b_(g)) / p
  data(Salamanders, package = "glmmTMB")
  m_tmb <- glmmTMB::glmmTMB(count ~ mined + (1 | site), family = poisson, data = Salamanders)
  b <- glmmTMB::fixef(m_tmb)$cond
  v_inv <- solve(as.matrix(stats::vcov(m_tmb)$cond))
  start <- m_tmb$obj$env$parList(m_tmb$fit$par)
  start <- start[!names(start) %in% c("b", "bzi", "bdisp") & lengths(start) > 0]
  sites <- levels(Salamanders$site)
  expected <- vapply(
    sites,
    function(s) {
      refit <- stats::update(m_tmb, data = Salamanders[Salamanders$site != s, ], start = start)
      delta <- b - glmmTMB::fixef(refit)$cond
      sum(delta * (v_inv %*% delta)) / length(b)
    },
    numeric(1)
  )
  actual <- .test_group_distances(m_tmb, "site")
  expect_length(actual, 23)
  expect_equal(actual[sites], expected, tolerance = 1e-6)
})


test_that("check_outliers, group result, default threshold, and print", {
  data(cbpp, package = "lme4")
  m <- lme4::glmer(
    cbind(incidence, size - incidence) ~ period + (1 | herd),
    family = binomial,
    data = cbpp
  )
  out <- check_outliers(m, group = "herd")
  out_data <- as.data.frame(out)
  n_groups <- nlevels(cbpp$herd)

  expect_identical(nrow(out_data), n_groups)
  expect_identical(as.character(out_data$Group), levels(cbpp$herd))
  expect_identical(attr(out, "threshold"), list(cook = 4 / n_groups))

  flagged <- as.character(out_data$Group[out_data$Distance_Cook > 4 / n_groups])
  expect_identical(as.character(out_data$Group[out]), flagged)
  expect_gte(length(flagged), 1)
  expect_output(
    print(out),
    paste0(
      length(flagged),
      " outliers detected: levels ",
      toString(flagged),
      "."
    ),
    fixed = TRUE
  )
  expect_output(print(out), "For grouping variable: herd.", fixed = TRUE)
})


test_that("check_outliers, group argument errors", {
  data(cbpp, package = "lme4")
  m <- lme4::glmer(
    cbind(incidence, size - incidence) ~ period + (1 | herd),
    family = binomial,
    data = cbpp
  )
  expect_error(
    check_outliers(m, group = "not_a_variable"),
    "not_a_variable",
    fixed = TRUE
  )
  expect_error(
    check_outliers(m, method = "zscore_robust", group = "herd"),
    "`group`",
    fixed = TRUE
  )
  expect_error(
    check_outliers(m, method = c("cook", "iqr"), group = "herd"),
    "`group`",
    fixed = TRUE
  )
  expect_error(
    check_outliers(stats::lm(mpg ~ wt, data = mtcars), group = "cyl"),
    "models of class `lm`",
    fixed = TRUE
  )
  # period has 4 levels and the model has 4 fixed effects
  expect_error(
    check_outliers(m, group = "period"),
    "(G = 4)",
    fixed = TRUE
  )
})
