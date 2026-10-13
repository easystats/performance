skip_if_not_installed("glmmTMB")
skip_if_not_installed("lme4")

# test-local Cook's distance: refit without each row, starting at the
# full-model estimates, then (b - b_(i))' V(b)^-1 (b - b_(i)) / p. A refit
# that errors, does not converge, or has a non-positive-definite Hessian
# gives NA.
.test_cooks_refit <- function(model, data) {
  b <- glmmTMB::fixef(model)$cond
  v_inv <- solve(as.matrix(stats::vcov(model)$cond))
  start <- model$obj$env$parList(model$fit$par)
  start <- start[!names(start) %in% c("b", "bzi", "bdisp") & lengths(start) > 0]
  out <- numeric(nrow(data))
  for (i in seq_len(nrow(data))) {
    refit <- try(
      suppressWarnings(stats::update(model, data = data[-i, ], start = start)),
      silent = TRUE
    )
    if (inherits(refit, "try-error") || refit$fit$convergence != 0 || !refit$sdr$pdHess) {
      out[i] <- NA
    } else {
      delta <- b - glmmTMB::fixef(refit)$cond
      out[i] <- sum(delta * (v_inv %*% delta)) / length(b)
    }
  }
  out
}


test_that("check_outliers, glmmTMB Cook's distance equals lm Cook's distance", {
  m <- glmmTMB::glmmTMB(mpg ~ wt + hp, data = mtcars, REML = TRUE)
  out <- check_outliers(m, method = "cook")
  expect_equal(
    as.data.frame(out)$Distance_Cook,
    unname(stats::cooks.distance(stats::lm(mpg ~ wt + hp, data = mtcars))),
    tolerance = 1e-6
  )
})


test_that("check_outliers, glmmTMB Cook's distance equals refit formula", {
  skip_on_cran()
  data(sleepstudy, package = "lme4")
  m1 <- glmmTMB::glmmTMB(Reaction ~ Days + (Days | Subject), data = sleepstudy)
  out1 <- check_outliers(m1, method = "cook")
  expect_equal(
    as.data.frame(out1)$Distance_Cook,
    .test_cooks_refit(m1, sleepstudy),
    tolerance = 1e-6
  )
  expect_identical(sum(is.na(as.data.frame(out1)$Distance_Cook)), 0L)

  data(Salamanders, package = "glmmTMB")
  d <- Salamanders[1:100, ]
  m2 <- glmmTMB::glmmTMB(
    count ~ mined + (1 | site),
    zi = ~mined,
    family = poisson,
    data = d
  )
  out2 <- suppressMessages(check_outliers(m2, method = "cook"))
  expect_equal(
    as.data.frame(out2)$Distance_Cook,
    .test_cooks_refit(m2, d),
    tolerance = 1e-6
  )
  expect_lte(sum(is.na(as.data.frame(out2)$Distance_Cook)), 2)
})


test_that("check_outliers, glmmTMB default method flags influential rows", {
  skip_on_cran()
  data(sleepstudy, package = "lme4")
  d <- sleepstudy
  d$Reaction[1] <- 1000
  m <- glmmTMB::glmmTMB(Reaction ~ Days + (1 | Subject), data = d)
  out <- check_outliers(m)
  expect_s3_class(out, "check_outliers")
  expect_true(out[1])
  out_data <- as.data.frame(out)
  expect_identical(
    which(as.vector(out)),
    which(out_data$Distance_Cook > attributes(out)$threshold$cook)
  )
})


data_methods <- list(
  zscore = NULL,
  zscore_robust = NULL,
  iqr = NULL,
  ci = "bayestestR",
  hdi = "bayestestR",
  eti = "bayestestR",
  bci = "bayestestR",
  mahalanobis = NULL,
  mahalanobis_robust = "bigutilsr",
  mcd = "MASS",
  ics = c("ICS", "ICSOutlier"),
  optics = "dbscan",
  lof = "dbscan"
)

for (method in names(data_methods)) {
  test_that(paste0("check_outliers, glmmTMB method '", method, "' equals lmer result"), {
    for (pkg in data_methods[[method]]) {
      skip_if_not_installed(pkg)
    }
    data(sleepstudy, package = "lme4")
    m_tmb <- glmmTMB::glmmTMB(Reaction ~ Days + (Days | Subject), data = sleepstudy)
    m_lmer <- lme4::lmer(Reaction ~ Days + (Days | Subject), data = sleepstudy)
    # "hdi" warns once per value, for lmer models too
    set.seed(123)
    out_tmb <- suppressWarnings(check_outliers(m_tmb, method = method))
    set.seed(123)
    out_lmer <- suppressWarnings(check_outliers(m_lmer, method = method))
    expect_identical(
      as.data.frame(out_tmb)$Outlier,
      as.data.frame(out_lmer)$Outlier
    )
  })
}


test_that("check_outliers, glmmTMB failed refits give NA and do not vote", {
  d <- mtcars
  d$mpg[5] <- 100 # extreme response, so the zscore_robust vote is 1
  m <- glmmTMB::glmmTMB(mpg ~ wt + hp, data = d)
  update_model <- .update_model

  # each case changes one thing about a real refit without the failing rows
  failures <- list(
    error = function(fit) stop("forced refit error", call. = FALSE),
    convergence = function(fit) {
      fit$fit$convergence <- 1L
      fit
    },
    pdHess = function(fit) {
      fit$sdr$pdHess <- FALSE
      fit
    }
  )
  cases <- list(
    list(failure = "error", rows = c(5L, 20L)),
    list(failure = "convergence", rows = c(5L, 20L)),
    list(failure = "pdHess", rows = c(5L, 20L)),
    list(failure = "pdHess", rows = 5L)
  )

  for (case in cases) {
    local_mocked_bindings(.update_model = function(x, data, start, weights, subset = NULL) {
      fit <- update_model(x, data = data, start = start, weights = weights, subset = subset)
      removed <- which(weights == 0)
      if (removed %in% case$rows) {
        fit <- failures[[case$failure]](fit)
      }
      fit
    })
    alerts <- character()
    out <- withCallingHandlers(
      check_outliers(m, method = c("cook", "zscore_robust")),
      message = function(msg) {
        alerts <<- c(alerts, conditionMessage(msg))
        invokeRestart("muffleMessage")
      }
    )
    out_data <- as.data.frame(out)
    info <- paste(case$failure, toString(case$rows))

    expect_true(all(is.na(out_data$Distance_Cook[case$rows])), info = info)
    expect_false(anyNA(out_data$Distance_Cook[-case$rows]), info = info)
    expect_identical(
      out_data$Outlier_Cook[case$rows],
      rep(0, length(case$rows)),
      info = info
    )
    expect_identical(
      out_data$Outlier[case$rows],
      out_data$Outlier_Zscore_robust[case$rows],
      info = info
    )
    expect_identical(out_data$Outlier_Zscore_robust[5], 1, info = info)

    cook_alerts <- grep("could not be computed", alerts, fixed = TRUE, value = TRUE)
    expect_length(cook_alerts, 1)
    if (length(case$rows) == 1) {
      expect_match(cook_alerts, "for row 5,", fixed = TRUE, info = info)
    } else {
      expect_match(cook_alerts, "for rows 5 and 20,", fixed = TRUE, info = info)
    }
  }
})


test_that("check_outliers, glmmTMB failed refits for all or many rows", {
  m <- glmmTMB::glmmTMB(mpg ~ wt + hp, data = mtcars)
  update_model <- .update_model

  local_mocked_bindings(.update_model = function(x, data, start, weights, subset = NULL) {
    stop("forced refit error", call. = FALSE)
  })
  expect_message(
    out <- check_outliers(m, method = "cook"),
    "could not be computed for any row",
    fixed = TRUE
  )
  expect_true(all(is.na(as.data.frame(out)$Distance_Cook)))
  expect_false(any(out))

  local_mocked_bindings(.update_model = function(x, data, start, weights, subset = NULL) {
    if (which(weights == 0) <= 12) {
      stop("forced refit error", call. = FALSE)
    }
    update_model(x, data = data, start = start, weights = weights, subset = subset)
  })
  alert <- tryCatch(
    check_outliers(m, method = "cook"),
    message = function(msg) gsub("\\s+", " ", conditionMessage(msg))
  )
  expect_match(alert, "rows 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 and 2 more,", fixed = TRUE)
})


test_that("check_outliers, glmmTMB Cook's distance with data-dependent terms", {
  m <- glmmTMB::glmmTMB(mpg ~ scale(wt) + poly(hp, 2), data = mtcars, REML = TRUE)
  expect_equal(
    as.data.frame(check_outliers(m, method = "cook"))$Distance_Cook,
    unname(stats::cooks.distance(stats::lm(
      mpg ~ scale(wt) + poly(hp, 2),
      data = mtcars
    ))),
    tolerance = 1e-6
  )
})


test_that("check_outliers, glmmTMB Cook's distance with external weights and offset", {
  w <- mtcars$cyl
  m1 <- glmmTMB::glmmTMB(mpg ~ wt + hp, data = mtcars, weights = w)
  out1 <- as.data.frame(check_outliers(m1, method = "cook"))$Distance_Cook
  m2 <- glmmTMB::glmmTMB(mpg ~ wt + hp, data = mtcars, weights = cyl)
  out2 <- as.data.frame(check_outliers(m2, method = "cook"))$Distance_Cook
  expect_false(anyNA(out1))
  expect_equal(out1, out2, tolerance = 1e-6)

  o <- log(mtcars$disp)
  m3 <- glmmTMB::glmmTMB(carb ~ wt + offset(o), family = poisson, data = mtcars)
  out3 <- as.data.frame(check_outliers(m3, method = "cook"))$Distance_Cook
  d <- mtcars
  d$o <- log(d$disp)
  m4 <- glmmTMB::glmmTMB(carb ~ wt + offset(o), family = poisson, data = d)
  out4 <- as.data.frame(check_outliers(m4, method = "cook"))$Distance_Cook
  expect_false(anyNA(out3))
  expect_equal(out3, out4, tolerance = 1e-6)
})


test_that("check_outliers, glmmTMB Cook's distance with dropped or mapped fixed effects", {
  d <- mtcars
  d$wt2 <- d$wt * 2
  m1 <- suppressWarnings(glmmTMB::glmmTMB(mpg ~ wt + wt2 + hp, data = d, REML = TRUE))
  # insight warns that the model matrix is rank deficient
  out1 <- as.data.frame(suppressWarnings(check_outliers(
    m1,
    method = "cook"
  )))$Distance_Cook
  expect_equal(
    out1,
    unname(stats::cooks.distance(stats::lm(mpg ~ wt + hp, data = d))),
    tolerance = 1e-6
  )

  m2 <- glmmTMB::glmmTMB(
    mpg ~ wt + hp,
    data = mtcars,
    map = list(beta = factor(c(1, NA, 2))),
    start = list(beta = c(30, -3, 0))
  )
  out2 <- as.data.frame(suppressWarnings(check_outliers(
    m2,
    method = "cook"
  )))$Distance_Cook
  expect_length(out2, nrow(mtcars))
  expect_false(anyNA(out2))
})


test_that("check_model, glmmTMB models do not run Cook's distance refits", {
  skip_if_not_installed("see")
  refit_calls <- 0
  local_mocked_bindings(.cooks_distance_refit = function(x) {
    refit_calls <<- refit_calls + 1
    rep(0, insight::n_obs(x))
  })
  data(sleepstudy, package = "lme4")
  m1 <- glmmTMB::glmmTMB(Reaction ~ Days + (1 | Subject), data = sleepstudy)
  out1 <- suppressWarnings(suppressMessages(check_model(m1, verbose = FALSE)))
  expect_s3_class(out1, "check_model")

  data(Salamanders, package = "glmmTMB")
  m2 <- glmmTMB::glmmTMB(count ~ mined + (1 | site), family = poisson, data = Salamanders)
  out2 <- suppressWarnings(suppressMessages(check_model(m2, verbose = FALSE)))
  expect_s3_class(out2, "check_model")

  expect_identical(refit_calls, 0)
})


test_that("check_outliers, unsupported robustbase classes return NULL", {
  # robustbase is not a dependency, and these methods only look at the class
  models <- list(
    structure(list(), class = "lmrob"),
    structure(list(), class = "glmrob")
  )
  for (model in models) {
    expect_message(
      expect_null(check_outliers(model)),
      paste0("does not yet support models of class `", class(model)[1]),
      fixed = TRUE
    )
  }
})


test_that("check_outliers, unsupported quantreg classes return NULL", {
  skip_if_not_installed("quantreg")
  data(stackloss)
  qss <- quantreg::qss
  models <- list(
    quantreg::rq(stack.loss ~ ., data = stackloss),
    quantreg::rq(stack.loss ~ ., data = stackloss, tau = c(0.25, 0.5)),
    quantreg::rqss(stack.loss ~ qss(Air.Flow), data = stackloss)
  )
  expect_identical(
    vapply(models, function(m) class(m)[1], character(1)),
    c("rq", "rqs", "rqss")
  )
  for (model in models) {
    expect_message(
      expect_null(check_outliers(model)),
      paste0("does not yet support models of class `", class(model)[1]),
      fixed = TRUE
    )
  }
})


test_that("check_outliers, glmmTMB Cook's distance for binomial models with a cbind() response", {
  skip_on_cran()
  # the number of trials must not be passed to the refits as prior weights.
  # `size` is a predictor because the data checks need a numeric predictor.
  data(cbpp, package = "lme4")
  m <- glmmTMB::glmmTMB(
    cbind(incidence, size - incidence) ~ period + size + (1 | herd),
    family = binomial,
    data = cbpp
  )
  out <- check_outliers(m, method = "cook")
  expect_equal(
    as.data.frame(out)$Distance_Cook,
    .test_cooks_refit(m, cbpp),
    tolerance = 1e-6
  )
})
