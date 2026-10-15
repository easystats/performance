skip_if_not_installed("glmmTMB")
skip_if_not_installed("lme4")

data(sleepstudy, package = "lme4")
data(cbpp, package = "lme4")
data(Salamanders, package = "glmmTMB")

# test-local hat values, recomputed with solve() as the diagonal of
# W^(1/2) A (A' W A + G)^(-1) A' W^(1/2), with A = [X, Z], W the GLM weight
# divided by the dispersion, and G zero for the fixed effects and the inverse
# random-effects covariance for the random effects (Lee & Nelder 1996, p. 631).
# `prior_weights` and `variance` are given by the test, not taken from the model.
.test_hat_glmmTMB <- function(model, prior_weights, variance) {
  X <- as.matrix(glmmTMB::getME(model, "X"))
  Z <- as.matrix(glmmTMB::getME(model, "Z"))
  fam <- stats::family(model)
  eta <- stats::predict(model, type = "link")
  mu <- fam$linkinv(eta)
  w <- prior_weights * fam$mu.eta(eta)^2 / variance(mu)

  vc <- glmmTMB::VarCorr(model)$cond
  gp <- glmmTMB::getME(model, "Gp")
  sigma_blocks <- lapply(seq_along(vc), function(i) {
    s <- matrix(unclass(vc[[i]]), nrow(vc[[i]]))
    kronecker(diag(diff(gp)[i] / nrow(s)), s)
  })
  sigma_re <- as.matrix(Matrix::bdiag(sigma_blocks))

  A <- cbind(X, Z)
  G <- matrix(0, ncol(A), ncol(A))
  re <- ncol(X) + seq_len(ncol(Z))
  G[re, re] <- solve(sigma_re)
  H <- solve(t(A) %*% (w * A) + G)
  diag((sqrt(w) * A) %*% H %*% t(sqrt(w) * A))
}

# one-step Cook's distance r^2 h / (p (1 - h)^2), with r the Pearson residual
# scaled by the dispersion
.test_cook <- function(y, mu, hat, prior_weights, variance, p) {
  r <- (y - mu) * sqrt(prior_weights) / sqrt(variance(mu))
  r^2 * hat / (p * (1 - hat)^2)
}

.influential <- function(model) {
  check_model(model, check = "outliers")$INFLUENTIAL
}

# the result of `check_model()` and the messages it gives
.check_model_messages <- function(model, ...) {
  msgs <- character()
  out <- withCallingHandlers(
    suppressWarnings(check_model(model, ...)),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  list(out = out, messages = msgs)
}


test_that("check_model, glmmTMB gaussian outlier panel equals lmer", {
  for (reml in c(FALSE, TRUE)) {
    m <- glmmTMB::glmmTMB(
      Reaction ~ Days + (Days | Subject),
      data = sleepstudy,
      REML = reml
    )
    g <- lme4::lmer(Reaction ~ Days + (Days | Subject), data = sleepstudy, REML = reml)
    out <- .influential(m)
    expect_equal(out$Hat, stats::hatvalues(g), tolerance = 1e-4, ignore_attr = TRUE)
    expect_equal(
      out$Cooks_Distance,
      stats::cooks.distance(g),
      tolerance = 1e-4,
      ignore_attr = TRUE
    )
  }
})


test_that("check_model, glmmTMB outlier panel without random effects equals glm", {
  skip_if_not_installed("MASS")
  fits <- list(
    poisson = list(
      glmmTMB::glmmTMB(count ~ mined + spp, family = poisson, data = Salamanders),
      stats::glm(count ~ mined + spp, family = poisson, data = Salamanders)
    ),
    binomial = list(
      glmmTMB::glmmTMB(
        cbind(incidence, size - incidence) ~ period,
        family = binomial,
        data = cbpp
      ),
      stats::glm(
        cbind(incidence, size - incidence) ~ period,
        family = binomial,
        data = cbpp
      )
    ),
    nbinom2 = list(
      glmmTMB::glmmTMB(
        count ~ mined + spp,
        family = glmmTMB::nbinom2,
        data = Salamanders
      ),
      MASS::glm.nb(count ~ mined + spp, data = Salamanders)
    )
  )
  for (f in fits) {
    out <- .influential(f[[1]])
    expect_equal(out$Hat, stats::hatvalues(f[[2]]), tolerance = 1e-6, ignore_attr = TRUE)
    expect_equal(
      out$Cooks_Distance,
      stats::cooks.distance(f[[2]]),
      tolerance = 1e-5,
      ignore_attr = TRUE
    )
    expect_equal(
      out$Std_Residuals,
      stats::rstandard(f[[2]], type = "pearson"),
      tolerance = 1e-5,
      ignore_attr = TRUE
    )
  }

  m <- glmmTMB::glmmTMB(Reaction ~ Days, family = Gamma(link = "log"), data = sleepstudy)
  g <- stats::glm(Reaction ~ Days, family = Gamma(link = "log"), data = sleepstudy)
  out <- .influential(m)
  expect_equal(out$Hat, stats::hatvalues(g), tolerance = 1e-6, ignore_attr = TRUE)
  expect_equal(
    out$Cooks_Distance,
    stats::cooks.distance(g, dispersion = stats::sigma(m)^2),
    tolerance = 1e-5,
    ignore_attr = TRUE
  )
})


test_that("check_model, glmmTMB outlier panel equals the hat-value formula for GLMMs", {
  v_poisson <- function(mu) mu
  v_binomial <- function(mu) mu * (1 - mu)
  fits <- list(
    list(
      model = glmmTMB::glmmTMB(
        count ~ mined + (1 | site),
        family = poisson,
        data = Salamanders
      ),
      y = Salamanders$count,
      w = rep(1, nrow(Salamanders)),
      v = v_poisson
    ),
    list(
      model = glmmTMB::glmmTMB(
        count ~ mined + (1 | site) + (1 | spp),
        family = poisson,
        data = Salamanders
      ),
      y = Salamanders$count,
      w = rep(1, nrow(Salamanders)),
      v = v_poisson
    ),
    list(
      model = glmmTMB::glmmTMB(
        cbind(incidence, size - incidence) ~ period + (1 | herd),
        family = binomial,
        data = cbpp
      ),
      y = cbpp$incidence / cbpp$size,
      w = cbpp$size,
      v = v_binomial
    ),
    list(
      model = glmmTMB::glmmTMB(
        incidence / size ~ period + (1 | herd),
        weights = size,
        family = binomial,
        data = cbpp
      ),
      y = cbpp$incidence / cbpp$size,
      w = cbpp$size,
      v = v_binomial
    )
  )
  m_nb <- glmmTMB::glmmTMB(
    count ~ mined + (1 | site),
    family = glmmTMB::nbinom2,
    data = Salamanders
  )
  theta <- stats::sigma(m_nb)
  fits[[5]] <- list(
    model = m_nb,
    y = Salamanders$count,
    w = rep(1, nrow(Salamanders)),
    v = function(mu) mu * (1 + mu / theta)
  )
  m_gamma <- glmmTMB::glmmTMB(
    Reaction ~ Days + (Days | Subject),
    family = Gamma(link = "log"),
    data = sleepstudy
  )
  phi <- stats::sigma(m_gamma)^2
  fits[[6]] <- list(
    model = m_gamma,
    y = sleepstudy$Reaction,
    w = rep(1, nrow(sleepstudy)),
    v = function(mu) phi * mu^2
  )

  for (f in fits) {
    out <- .influential(f$model)
    hat <- .test_hat_glmmTMB(f$model, f$w, f$v)
    expect_equal(out$Hat, hat, tolerance = 1e-6, ignore_attr = TRUE)
    mu <- stats::predict(f$model, type = "response")
    p <- ncol(glmmTMB::getME(f$model, "X"))
    expect_equal(
      out$Cooks_Distance,
      .test_cook(f$y, mu, hat, f$w, f$v, p),
      tolerance = 1e-6,
      ignore_attr = TRUE
    )
  }

  # with a random-effect SD near 0, the hat values are those of the glm
  m <- glmmTMB::glmmTMB(
    count ~ mined + (1 | site),
    family = poisson,
    data = Salamanders,
    start = list(theta = -10),
    map = list(theta = factor(NA))
  )
  g <- stats::glm(count ~ mined, family = poisson, data = Salamanders)
  expect_equal(
    .influential(m)$Hat,
    stats::hatvalues(g),
    tolerance = 1e-6,
    ignore_attr = TRUE
  )
})


test_that("check_model, glmmTMB outlier panel needs no refits", {
  refit_calls <- 0
  update_calls <- 0
  local_mocked_bindings(
    .cooks_distance_refit = function(...) {
      refit_calls <<- refit_calls + 1
      NULL
    },
    .update_model = function(...) {
      update_calls <<- update_calls + 1
      NULL
    }
  )
  m <- glmmTMB::glmmTMB(count ~ mined + (1 | site), family = poisson, data = Salamanders)
  out <- suppressWarnings(suppressMessages(check_model(m)))
  expect_false(is.null(out$INFLUENTIAL))
  expect_equal(
    attr(out$INFLUENTIAL, "cook_levels"),
    stats::qf(0.5, 2, 642),
    tolerance = 1e-10
  )
  expect_identical(refit_calls, 0)
  expect_identical(update_calls, 0)
})


test_that("check_model, glmmTMB outlier panel is skipped with a reason for unsupported fits", {
  fits <- list(
    "zero-inflation" = glmmTMB::glmmTMB(
      count ~ mined + (1 | site),
      ziformula = ~1,
      family = poisson,
      data = Salamanders
    ),
    "dispersion formula" = glmmTMB::glmmTMB(
      Reaction ~ Days + (1 | Subject),
      dispformula = ~Days,
      data = sleepstudy
    ),
    nbinom1 = glmmTMB::glmmTMB(
      count ~ mined + (1 | site),
      family = glmmTMB::nbinom1,
      data = Salamanders
    )
  )
  for (reason in names(fits)) {
    res <- .check_model_messages(fits[[reason]], verbose = TRUE)
    expect_null(res$out$INFLUENTIAL)
    skip_msg <- grep(
      "influential observations is skipped",
      res$messages,
      value = TRUE,
      fixed = TRUE
    )
    expect_length(skip_msg, 1)
    # the alert wraps its lines
    expect_match(gsub("\\s+", " ", skip_msg), reason, fixed = TRUE)

    res <- .check_model_messages(fits[[reason]])
    expect_null(res$out$INFLUENTIAL)
    expect_false(any(grepl("influential observations", res$messages, fixed = TRUE)))
  }
})


test_that("check_model, glmmTMB outlier panel is skipped for random effects in other model parts", {
  fits <- list(
    "zero-inflation" = glmmTMB::glmmTMB(
      count ~ mined + (1 | site),
      ziformula = ~ 0 + (1 | spp),
      family = poisson,
      data = Salamanders
    ),
    "dispersion formula" = glmmTMB::glmmTMB(
      Reaction ~ Days,
      dispformula = ~ 1 + (1 | Subject),
      data = sleepstudy
    )
  )
  for (reason in names(fits)) {
    res <- suppressWarnings(.check_model_messages(fits[[reason]], verbose = TRUE))
    expect_null(res$out$INFLUENTIAL)
    skip_msg <- grep(
      "influential observations is skipped",
      res$messages,
      value = TRUE,
      fixed = TRUE
    )
    expect_length(skip_msg, 1)
    expect_match(gsub("\\s+", " ", skip_msg), reason, fixed = TRUE)
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


test_that("check_model, glmmTMB outlier panel with na.exclude", {
  d <- sleepstudy
  d$Reaction[c(3, 50)] <- NA
  m <- glmmTMB::glmmTMB(
    Reaction ~ Days + (Days | Subject),
    data = d,
    na.action = na.exclude
  )
  g <- lme4::lmer(Reaction ~ Days + (Days | Subject), data = d, REML = FALSE)
  out <- .influential(m)
  expect_identical(nrow(out), 178L)
  expect_equal(out$Hat, stats::hatvalues(g), tolerance = 1e-4, ignore_attr = TRUE)
  expect_equal(out$Residuals, stats::residuals(g), tolerance = 1e-4, ignore_attr = TRUE)
})


test_that("check_model, glmmTMB outlier panel is skipped without fixed effects or dispersion", {
  reasons <- c(
    "no estimated fixed effects",
    "no estimated fixed effects",
    "fixed dispersion"
  )
  fits <- list(
    glmmTMB::glmmTMB(
      count ~ 0 + (1 | site),
      family = poisson,
      data = Salamanders
    ),
    # all fixed effects fixed by map, and no random effect
    glmmTMB::glmmTMB(
      count ~ mined,
      family = poisson,
      data = Salamanders,
      start = list(beta = c(0, 1)),
      map = list(beta = factor(c(NA, NA)))
    ),
    glmmTMB::glmmTMB(
      Reaction ~ Days + (1 | Subject),
      dispformula = ~0,
      data = sleepstudy
    )
  )
  for (i in seq_along(fits)) {
    res <- suppressWarnings(.check_model_messages(fits[[i]], verbose = TRUE))
    expect_null(res$out$INFLUENTIAL)
    skip_msg <- grep(
      "influential observations is skipped",
      res$messages,
      value = TRUE,
      fixed = TRUE
    )
    expect_length(skip_msg, 1)
    expect_match(gsub("\\s+", " ", skip_msg), reasons[i], fixed = TRUE)
  }
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
  expect_equal(out$Hat, .influential(m0)$Hat, tolerance = 1e-5)
  expect_identical(attr(out, "n_params"), 2L)
})


test_that("check_model, glmmTMB outlier panel alerts when hat values fail", {
  local_mocked_bindings(.model_diagnostic_outlier = function(...) {
    stop("failed", call. = FALSE)
  })
  m <- glmmTMB::glmmTMB(count ~ mined + (1 | site), family = poisson, data = Salamanders)
  res <- .check_model_messages(m, check = "outliers", verbose = TRUE)
  expect_null(res$out$INFLUENTIAL)
  skip_msg <- grep(
    "influential observations is skipped",
    res$messages,
    value = TRUE,
    fixed = TRUE
  )
  expect_length(skip_msg, 1)
  expect_match(gsub("\\s+", " ", skip_msg), "could not be computed", fixed = TRUE)
})
