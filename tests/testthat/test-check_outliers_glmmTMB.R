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


test_that("check_outliers, unsupported classes return NULL", {
  data(stackloss)
  models <- list()
  if (requireNamespace("robustbase", quietly = TRUE)) {
    models$lmrob <- robustbase::lmrob(stack.loss ~ ., data = stackloss)
    models$glmrob <- robustbase::glmrob(
      am ~ wt,
      family = binomial,
      data = mtcars
    )
  }
  if (requireNamespace("quantreg", quietly = TRUE)) {
    models$rq <- quantreg::rq(stack.loss ~ ., data = stackloss)
    models$rqs <- quantreg::rq(stack.loss ~ ., data = stackloss, tau = c(0.25, 0.5))
    qss <- quantreg::qss
    models$rqss <- quantreg::rqss(stack.loss ~ qss(Air.Flow), data = stackloss)
  }
  skip_if(length(models) == 0)
  for (model in models) {
    expect_message(
      expect_null(check_outliers(model)),
      paste0("does not yet support models of class `", class(model)[1]),
      fixed = TRUE
    )
  }
})
