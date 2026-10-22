skip_if_not_installed("glmmTMB")
skip_if_not_installed("lme4")

data(sleepstudy, package = "lme4")
data(cbpp, package = "lme4")
data(Salamanders, package = "glmmTMB")

.influential <- function(model) {
  check_model(model, check = "outliers")$INFLUENTIAL
}


test_that("check_model, glmmTMB outlier panel with na.exclude equals lmer", {
  d <- sleepstudy
  d$Reaction[c(3, 50)] <- NA
  m <- glmmTMB::glmmTMB(
    Reaction ~ Days + (Days | Subject),
    data = d,
    na.action = na.exclude
  )
  g <- lme4::lmer(Reaction ~ Days + (Days | Subject), data = d, REML = FALSE)
  out <- .influential(m)
  expect_equal(out$Hat, stats::hatvalues(g), tolerance = 1e-4, ignore_attr = TRUE)
  expect_equal(
    out$Cooks_Distance,
    stats::cooks.distance(g),
    tolerance = 1e-4,
    ignore_attr = TRUE
  )
  expect_equal(attr(out, "cook_levels"), stats::qf(0.5, 2, 176), tolerance = 1e-10)
})


test_that("check_model, glmmTMB outlier panel without random effects equals glm", {
  m <- glmmTMB::glmmTMB(
    cbind(incidence, size - incidence) ~ period,
    family = binomial,
    data = cbpp
  )
  g <- stats::glm(
    cbind(incidence, size - incidence) ~ period,
    family = binomial,
    data = cbpp
  )
  out <- .influential(m)
  expect_equal(out$Hat, stats::hatvalues(g), tolerance = 1e-6, ignore_attr = TRUE)
  expect_equal(
    out$Cooks_Distance,
    stats::cooks.distance(g),
    tolerance = 1e-5,
    ignore_attr = TRUE
  )
  expect_equal(
    out$Std_Residuals,
    stats::rstandard(g, type = "pearson"),
    tolerance = 1e-5,
    ignore_attr = TRUE
  )
})


test_that("check_model, glmmTMB outlier panel is skipped with a reason for unsupported fits", {
  skip_on_cran()
  fits <- suppressWarnings(list(
    "zero-inflation" = glmmTMB::glmmTMB(
      count ~ mined + (1 | site),
      ziformula = ~1,
      family = poisson,
      data = Salamanders
    ),
    "a dispersion formula" = glmmTMB::glmmTMB(
      Reaction ~ Days + (1 | Subject),
      dispformula = ~Days,
      data = sleepstudy
    ),
    "a fixed dispersion" = glmmTMB::glmmTMB(
      Reaction ~ Days + (1 | Subject),
      dispformula = ~0,
      data = sleepstudy
    ),
    "the `nbinom1` family" = glmmTMB::glmmTMB(
      count ~ mined + (1 | site),
      family = glmmTMB::nbinom1,
      data = Salamanders
    ),
    "no estimated fixed effects" = glmmTMB::glmmTMB(
      count ~ 0 + (1 | site),
      family = poisson,
      data = Salamanders
    )
  ))
  for (reason in names(fits)) {
    msgs <- capture_messages(
      out <- suppressWarnings(check_model(
        fits[[reason]],
        check = "outliers",
        verbose = TRUE
      ))
    )
    expect_null(out$INFLUENTIAL)
    expect_match(
      gsub("\\s+", " ", msgs),
      paste("not available for models with", reason),
      fixed = TRUE,
      all = FALSE
    )
  }
})


test_that("check_model, glmmTMB outlier panel leaves out fixed effects that map fixes", {
  m <- glmmTMB::glmmTMB(
    count ~ mined,
    family = poisson,
    data = Salamanders,
    map = list(beta = factor(c(NA, 1))),
    start = list(beta = c(-1, 0))
  )
  # the intercept is fixed at -1, so only the coefficient of `minedno` is estimated
  g <- stats::glm(
    count ~ 0 + I(as.numeric(mined == "no")),
    family = poisson,
    data = Salamanders,
    offset = rep(-1, nrow(Salamanders))
  )
  out <- .influential(m)
  expect_equal(out$Hat, stats::hatvalues(g), tolerance = 1e-6, ignore_attr = TRUE)
  expect_identical(attr(out, "n_params"), 1L)
})


test_that("check_model, glmmTMB outlier panel with an aliased column", {
  d <- Salamanders
  d$mined2 <- d$mined
  # without the rank check, X keeps the aliased column
  m <- suppressWarnings(glmmTMB::glmmTMB(
    count ~ mined + mined2 + (1 | site),
    family = poisson,
    data = d,
    control = glmmTMB::glmmTMBControl(rank_check = "skip")
  ))
  m0 <- glmmTMB::glmmTMB(count ~ mined + (1 | site), family = poisson, data = d)
  out <- suppressWarnings(.influential(m))
  expect_equal(out$Hat, .influential(m0)$Hat, tolerance = 1e-4)
  expect_identical(attr(out, "n_params"), 2L)
})


test_that("check_model, glmmTMB outlier panel alerts when hat values fail", {
  local_mocked_bindings(.model_diagnostic_outlier = function(...) {
    stop("failed", call. = FALSE)
  })
  m <- glmmTMB::glmmTMB(count ~ mined + (1 | site), family = poisson, data = Salamanders)
  msgs <- capture_messages(out <- check_model(m, check = "outliers", verbose = TRUE))
  expect_null(out$INFLUENTIAL)
  expect_match(
    gsub("\\s+", " ", msgs),
    "could not be computed",
    fixed = TRUE,
    all = FALSE
  )
})


test_that("check_model, glmmTMB outlier panel is skipped when the threshold fails", {
  panel_calls <- 0
  local_mocked_bindings(
    .check_outliers_cook_threshold = function(...) {
      stop("n - p < 1", call. = FALSE)
    },
    .model_diagnostic_outlier = function(...) {
      panel_calls <<- panel_calls + 1
      NULL
    }
  )
  m <- glmmTMB::glmmTMB(count ~ mined + (1 | site), family = poisson, data = Salamanders)
  out <- suppressMessages(check_model(m, check = "outliers", verbose = TRUE))
  expect_null(out$INFLUENTIAL)
  expect_identical(panel_calls, 0)
})
