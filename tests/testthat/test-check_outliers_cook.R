# test-local Cook's distance for glmmTMB models: refit the model to `data`
# without each row, with no start values, then
# (b - b_(i))' V(b)^-1 (b - b_(i)) / p, with b and V(b) the conditional fixed
# effects of `model`. `refit` fits the model to a data frame. `data` holds
# precomputed columns in place of terms like scale() or poly(), and weights or
# offsets as columns.
.test_cooks_refit_loop <- function(model, data, refit) {
  b <- unname(glmmTMB::fixef(model)$cond)
  v_inv <- solve(as.matrix(stats::vcov(model)$cond))
  vapply(
    seq_len(nrow(data)),
    function(i) {
      fit_i <- suppressWarnings(refit(data[-i, ]))
      delta <- b - unname(glmmTMB::fixef(fit_i)$cond)
      sum(delta * (v_inv %*% delta)) / length(b)
    },
    numeric(1)
  )
}

# muffle the warnings of lme4 about hat values of GLMMs, from
# cooks.distance() and rstandard()
.muffle_lme4_hat_warnings <- function(expr) {
  withCallingHandlers(
    expr,
    warning = function(w) {
      if (grepl("hat matrix may not make sense|NaNs produced", conditionMessage(w))) {
        invokeRestart("muffleWarning")
      }
    }
  )
}


test_that("check_outliers, Cook's distance with data methods that flag no row", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("glmmTMB")
  data(sleepstudy, package = "lme4")
  models <- list(
    lm = lm(mpg ~ disp + drat, data = mtcars),
    lmer = lme4::lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy),
    glmmTMB = glmmTMB::glmmTMB(Reaction ~ Days + (1 | Subject), data = sleepstudy)
  )
  for (m in models) {
    cook_only <- as.data.frame(check_outliers(m, method = "cook"))
    for (data_method in c("zscore_robust", "iqr")) {
      # the data method alone flags no row
      expect_false(any(check_outliers(m, method = data_method)))

      out <- as.data.frame(check_outliers(m, method = c("cook", data_method)))
      expect_equal(out$Distance_Cook, cook_only$Distance_Cook, tolerance = 1e-10)
      expect_equal(out$Outlier, cook_only$Outlier_Cook / 2, tolerance = 1e-10)
    }
    out <- as.data.frame(check_outliers(m, method = c("cook", "zscore_robust", "iqr")))
    expect_equal(out$Distance_Cook, cook_only$Distance_Cook, tolerance = 1e-10)
  }
})


test_that("check_outliers, Cook's distance for binomial models with only factor predictors", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("glmmTMB")
  data(cbpp, package = "lme4")
  m_glm <- glm(cbind(incidence, size - incidence) ~ period, family = binomial, data = cbpp)
  m_glmer <- lme4::glmer(
    cbind(incidence, size - incidence) ~ period + (1 | herd),
    family = binomial,
    data = cbpp
  )
  m_glmmtmb <- glmmTMB::glmmTMB(
    cbind(incidence, size - incidence) ~ period + (1 | herd),
    family = binomial,
    data = cbpp
  )

  for (m in list(m_glm, m_glmer)) {
    out <- .muffle_lme4_hat_warnings(as.data.frame(check_outliers(m, method = "cook")))
    expect_length(out$Distance_Cook, 56)
    expect_equal(
      out$Distance_Cook,
      .muffle_lme4_hat_warnings(unname(stats::cooks.distance(m))),
      tolerance = 1e-8
    )
  }

  skip_on_cran()
  out <- as.data.frame(check_outliers(m_glmmtmb, method = "cook"))
  expect_length(out$Distance_Cook, 56)
  oracle <- .test_cooks_refit_loop(m_glmmtmb, cbpp, function(data_i) {
    glmmTMB::glmmTMB(
      cbind(incidence, size - incidence) ~ period + (1 | herd),
      family = binomial,
      data = data_i
    )
  })
  expect_lte(max(abs(out$Distance_Cook - oracle)), 1e-4)

  # the data methods still need numeric variables
  expect_error(
    check_outliers(m_glm, method = c("cook", "zscore")),
    regex = "No numeric variables found"
  )
})


test_that("check_outliers, default Cook's distance threshold uses p and n of the model", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("glmmTMB")
  data(sleepstudy, package = "lme4")
  data(cbpp, package = "lme4")
  d <- mtcars
  d$wt2 <- 2 * d$wt

  m_factor <- lm(mpg ~ factor(cyl) + wt, data = mtcars)
  m_poly <- lm(mpg ~ poly(wt, 3), data = mtcars)
  m_lm <- lm(mpg ~ wt + hp, data = mtcars)
  m_aliased <- lm(mpg ~ wt + wt2, data = d)
  m_glm <- glm(cbind(incidence, size - incidence) ~ period, family = binomial, data = cbpp)
  m_lmer <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
  m_glmmtmb <- glmmTMB::glmmTMB(Reaction ~ poly(Days, 2) + (1 | Subject), data = sleepstudy)

  # p and n computed independently of performance
  expected <- function(m, p) stats::qf(0.5, p, stats::nobs(m) - p)
  cook_threshold <- function(m, method = "cook") {
    attr(check_outliers(m, method = method), "threshold")$cook
  }

  for (m in list(m_factor, m_poly, m_lm, m_aliased, m_glm)) {
    expect_equal(cook_threshold(m), expected(m, sum(!is.na(stats::coef(m)))), tolerance = 1e-12)
  }
  expect_equal(cook_threshold(m_lmer), expected(m_lmer, length(lme4::fixef(m_lmer))), tolerance = 1e-12)
  expect_equal(
    cook_threshold(m_glmmtmb),
    expected(m_glmmtmb, length(glmmTMB::fixef(m_glmmtmb)$cond)),
    tolerance = 1e-12
  )
  expect_equal(
    cook_threshold(m_poly, method = c("cook", "zscore_robust")),
    expected(m_poly, sum(!is.na(stats::coef(m_poly)))),
    tolerance = 1e-12
  )
  # unchanged for a model whose numeric data columns match its fixed effects
  expect_equal(cook_threshold(m_lm), stats::qf(0.5, 3, 29), tolerance = 1e-12)
})


test_that("check_outliers, glmmTMB Cook's distance refits use the data given to the model", {
  skip_on_cran()
  skip_if_not_installed("lme4")
  skip_if_not_installed("glmmTMB")
  data(sleepstudy, package = "lme4")

  # missing response values with a data-dependent term
  d <- sleepstudy
  d$Reaction[c(5, 50)] <- NA
  m1 <- glmmTMB::glmmTMB(Reaction ~ scale(Days) + (1 | Subject), data = d)
  d1 <- d
  d1$Days_scaled <- as.vector(scale(d1$Days))
  d1 <- d1[!is.na(d1$Reaction), ]
  oracle1 <- .test_cooks_refit_loop(m1, d1, function(data_i) {
    glmmTMB::glmmTMB(Reaction ~ Days_scaled + (1 | Subject), data = data_i)
  })

  # weights defined outside the data, with an index subset
  set.seed(1)
  w <- runif(180, 0.5, 2)
  m2 <- glmmTMB::glmmTMB(
    Reaction ~ Days + (1 | Subject),
    data = sleepstudy,
    weights = w,
    subset = 1:150
  )
  d2 <- sleepstudy
  d2$w <- w
  d2 <- d2[1:150, ]
  oracle2 <- .test_cooks_refit_loop(m2, d2, function(data_i) {
    glmmTMB::glmmTMB(Reaction ~ Days + (1 | Subject), data = data_i, weights = w)
  })

  # poly() and an offset defined outside the data, with missing responses
  o <- log(mtcars$disp)
  d3 <- mtcars
  d3$carb[c(2, 9)] <- NA
  m3 <- glmmTMB::glmmTMB(carb ~ poly(wt, 2) + offset(o), family = poisson, data = d3)
  d3_oracle <- d3
  poly_wt <- stats::poly(d3_oracle$wt, 2)
  d3_oracle$wt1 <- poly_wt[, 1]
  d3_oracle$wt2 <- poly_wt[, 2]
  d3_oracle$o <- o
  d3_oracle <- d3_oracle[!is.na(d3_oracle$carb), ]
  oracle3 <- .test_cooks_refit_loop(m3, d3_oracle, function(data_i) {
    glmmTMB::glmmTMB(carb ~ wt1 + wt2 + offset(o), family = poisson, data = data_i)
  })

  models <- list(m1, m2, m3)
  oracles <- list(oracle1, oracle2, oracle3)
  for (i in seq_along(models)) {
    out <- as.data.frame(check_outliers(models[[i]], method = "cook"))
    expect_identical(nrow(out), as.integer(stats::nobs(models[[i]])))
    expect_true(all(is.finite(out$Distance_Cook)))
    expect_lte(max(abs(out$Distance_Cook - oracles[[i]])), 1e-4)
  }
})


test_that("check_outliers, glmmTMB Cook's distance for a model fitted without data", {
  skip_if_not_installed("glmmTMB")
  set.seed(2)
  x <- rnorm(30)
  y <- x + rnorm(30)
  # glmmTMB recommends the `data` argument
  m <- suppressWarnings(glmmTMB::glmmTMB(y ~ x))
  out <- as.data.frame(check_outliers(m, method = "cook"))
  expect_identical(nrow(out), 30L)
  expect_true(all(is.finite(out$Distance_Cook)))
})


test_that("check_outliers, glmmTMB refits rejected with start values are tried without", {
  skip_on_cran()
  skip_if_not_installed("glmmTMB")
  set.seed(18)
  d <- data.frame(g = factor(rep(1:10, each = 5)), x = rnorm(50))
  d$y <- 1 + 0.5 * d$x + rnorm(50) + rep(rnorm(10, sd = 0.15), each = 5)
  m <- glmmTMB::glmmTMB(y ~ x + (1 | g), data = d)

  # the case needs a refit that fails with the full model's estimates as
  # start values, which depends on the optimizer
  start <- m$obj$env$parList(m$fit$par)
  start <- start[!names(start) %in% c("b", "bzi", "bdisp") & lengths(start) > 0]
  warm_rejected <- vapply(
    seq_len(nrow(d)),
    function(i) {
      w <- rep(1, nrow(d))
      w[i] <- 0
      refit <- try(
        suppressWarnings(stats::update(m, data = d, weights = w, start = start)),
        silent = TRUE
      )
      inherits(refit, "try-error") || !isTRUE(refit$sdr$pdHess)
    },
    logical(1)
  )
  skip_if_not(any(warm_rejected), "No warm-started refit has a non-positive-definite Hessian.")

  out <- as.data.frame(check_outliers(m, method = "cook"))
  expect_length(out$Distance_Cook, 50)
  expect_true(all(is.finite(out$Distance_Cook)))
  oracle <- .test_cooks_refit_loop(m, d, function(data_i) {
    glmmTMB::glmmTMB(y ~ x + (1 | g), data = data_i)
  })
  expect_lte(max(abs(out$Distance_Cook - oracle)), 1e-4)
})

