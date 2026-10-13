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
  # this balanced design. On unbalanced data the estimates still agree, but
  # single entries of the REML covariance matrices of the fixed effects differ
  # by up to about 5%, and the distances by up to about 1% (measured on the
  # unbalanced data of the lmer test above, with lme4 2.0.6 and glmmTMB
  # 1.1.15.2, 2026-10-12). So the tolerance only holds for balanced data.
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
  skip_on_cran()
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


test_that("check_outliers, group refits keep the rows and terms of the model", {
  skip_on_cran()
  # the model drops rows by `subset` and by missing values, and computes
  # scale() on all rows of the data. Test-local loop: refit on the same data
  # with the rows of the model, without the rows of each level.
  data(sleepstudy, package = "lme4")
  d <- sleepstudy
  d$Reaction[c(3, 50)] <- NA
  rows_out <- -seq(1, 180, 3)
  loop_distances <- function(model, fixed, varcov) {
    model_rows <- rownames(d) %in% rownames(stats::model.frame(model))
    b <- fixed(model)
    v_inv <- solve(varcov(model))
    start <- .cooks_refit_start(model)
    vapply(
      levels(d$Subject),
      function(s) {
        d_keep <- d
        d_keep$keep <- model_rows & d$Subject != s
        refit <- suppressWarnings(stats::update(model, data = d_keep, subset = keep, start = start))
        delta <- b - fixed(refit)
        sum(delta * (v_inv %*% delta)) / length(b)
      },
      numeric(1)
    )
  }

  m_lmer <- lme4::lmer(Reaction ~ scale(Days) + (1 | Subject), d, subset = rows_out)
  expected <- loop_distances(m_lmer, lme4::fixef, function(m) as.matrix(stats::vcov(m)))
  actual <- .test_group_distances(m_lmer, "Subject")
  expect_equal(actual[names(expected)], expected, tolerance = 1e-6)

  m_tmb <- glmmTMB::glmmTMB(Reaction ~ scale(Days) + (1 | Subject), d, subset = rows_out)
  expected <- loop_distances(
    m_tmb,
    function(m) glmmTMB::fixef(m)$cond,
    function(m) as.matrix(stats::vcov(m)$cond)
  )
  actual <- .test_group_distances(m_tmb, "Subject")
  expect_equal(actual[names(expected)], expected, tolerance = 1e-6)
})


test_that("check_outliers, group levels and variables of the data", {
  data(sleepstudy, package = "lme4")
  d <- sleepstudy
  d$cluster <- rep(1:20, 9)
  d$Reaction[1:3] <- NA
  w <- rep(c(1, 2), 90)

  # a variable that is not in the formula, a dot formula, missing values, and
  # a weights vector that is not in the data
  m <- lme4::lmer(Reaction ~ . - Subject - cluster + (1 | Subject), d, weights = w)
  out <- as.data.frame(check_outliers(m, group = "cluster"))
  expect_identical(out$Group, 1:20)
  expect_false(anyNA(out$Distance_Cook))

  # levels without rows in the model are not counted
  m_tmb <- glmmTMB::glmmTMB(Reaction ~ Days + (1 | Subject), sleepstudy, subset = -(1:10))
  out <- check_outliers(m_tmb, group = "Subject")
  expect_identical(nrow(as.data.frame(out)), 17L)
  expect_false("308" %in% as.character(as.data.frame(out)$Group))
  expect_identical(attr(out, "threshold"), list(cook = 4 / 17))
})


test_that("check_outliers, group arguments", {
  data(cbpp, package = "lme4")
  m <- lme4::glmer(
    cbind(incidence, size - incidence) ~ period + (1 | herd),
    family = binomial,
    data = cbpp
  )
  default <- check_outliers(m, group = "herd")
  expect_identical(
    as.data.frame(check_outliers(m, group = "herd", method = c("cook", "pareto"))),
    as.data.frame(default)
  )
  expect_identical(attr(check_outliers(m, group = "herd", threshold = 0.1), "threshold"), list(cook = 0.1))
  expect_identical(
    attr(check_outliers(m, group = "herd", threshold = list(cook = 0.1)), "threshold"),
    list(cook = 0.1)
  )
  expect_error(
    check_outliers(m, group = "herd", threshold = list(cook = "a")),
    "a list with a numeric `cook` element",
    fixed = TRUE
  )
  expect_error(
    check_outliers(m, group = "herd", threshold = list(cook = c(0.1, 1))),
    "a list with a numeric `cook` element",
    fixed = TRUE
  )
  expect_warning(
    check_outliers(m, group = "herd", ID = "herd"),
    "The `ID` argument is not used",
    fixed = TRUE
  )
})


test_that("check_outliers, group on classes with their own method", {
  skip_if_not_installed("nlme")
  data(sleepstudy, package = "lme4")
  m <- nlme::lme(Reaction ~ Days, random = ~ 1 | Subject, data = sleepstudy)
  expect_error(
    check_outliers(m, group = "Subject"),
    "models of class `lme`",
    fixed = TRUE
  )
})


test_that("check_outliers, group alerts and print for failed refits", {
  data(sleepstudy, package = "lme4")
  m <- lme4::lmer(Reaction ~ Days + (1 | Subject), sleepstudy)
  update_model <- .update_model

  local_mocked_bindings(.update_model = function(x, data, start, weights = NULL, subset = NULL) {
    stop("forced refit error", call. = FALSE)
  })
  expect_message(
    out <- check_outliers(m, group = "Subject"),
    "could not be computed for any level of `Subject`",
    fixed = TRUE
  )
  expect_true(all(is.na(as.data.frame(out)$Distance_Cook)))
  expect_false(any(out))
  expect_output(
    print(out),
    "Cook's distance could not be computed for any level of grouping variable Subject.",
    fixed = TRUE
  )

  # refits without the first 12 levels fail
  failing <- levels(sleepstudy$Subject)[1:12]
  local_mocked_bindings(.update_model = function(x, data, start, weights = NULL, subset = NULL) {
    if (any(data$Subject[!subset] %in% failing)) {
      stop("forced refit error", call. = FALSE)
    }
    update_model(x, data = data, start = start, weights = weights, subset = subset)
  })
  alert <- tryCatch(
    check_outliers(m, group = "Subject"),
    message = function(msg) gsub("\\s+", " ", conditionMessage(msg))
  )
  expect_match(
    alert,
    paste("levels", toString(failing[1:10]), "and 2 more of `Subject`"),
    fixed = TRUE
  )
})


test_that("check_outliers, print of grouped data frames is unchanged", {
  out <- check_outliers(datawizard::data_group(iris, "Species"), method = c("zscore", "iqr"))
  expect_null(attr(out, "group", exact = TRUE))
  expect_output(print(out), "outlier detected: case 44.", fixed = TRUE)
  expect_output(print(out), "For variables: Sepal.Length", fixed = TRUE)
})


test_that("check_outliers, group glmmTMB with weights and an offset outside the data", {
  skip_on_cran()
  data(Salamanders, package = "glmmTMB")
  w <- rep(c(1, 2), length.out = nrow(Salamanders))
  o <- rep(c(0, 0.5), length.out = nrow(Salamanders))
  m <- glmmTMB::glmmTMB(
    count ~ mined + offset(o) + (1 | site),
    family = poisson,
    data = Salamanders,
    weights = w
  )
  # test-local loop: refit on the rows without each site, with the weights and
  # the offset of those rows
  b <- glmmTMB::fixef(m)$cond
  v_inv <- solve(as.matrix(stats::vcov(m)$cond))
  start <- .cooks_refit_start(m)
  sites <- levels(Salamanders$site)
  expected <- vapply(
    sites,
    function(s) {
      keep <- Salamanders$site != s
      d_keep <- Salamanders[keep, ]
      d_keep$w_keep <- w[keep]
      d_keep$o_keep <- o[keep]
      refit <- glmmTMB::glmmTMB(
        count ~ mined + offset(o_keep) + (1 | site),
        family = poisson,
        data = d_keep,
        weights = w_keep,
        start = start
      )
      delta <- b - glmmTMB::fixef(refit)$cond
      sum(delta * (v_inv %*% delta)) / length(b)
    },
    numeric(1)
  )
  actual <- .test_group_distances(m, "site")
  expect_equal(actual[sites], expected, tolerance = 1e-6)
})


test_that("check_outliers, group data checks", {
  data(sleepstudy, package = "lme4")
  d <- sleepstudy
  m <- lme4::lmer(Reaction ~ Days + (1 | Subject), d)
  expect_error(
    check_outliers(m, group = factor("Subject")),
    "`group` must be a single character string",
    fixed = TRUE
  )

  # the data changed after the model was fitted
  d$Reaction <- d$Reaction * 2
  expect_error(
    check_outliers(m, group = "Subject"),
    "is not the data the model was fitted on",
    fixed = TRUE
  )
  d <- sleepstudy[-(1:30), ]
  expect_error(
    check_outliers(m, group = "Subject"),
    "is not the data the model was fitted on",
    fixed = TRUE
  )

  # the data cannot be found from the environment of the formula
  f <- Reaction ~ Days + (1 | Subject)
  fit_local <- function(formula) {
    local_data <- sleepstudy
    lme4::lmer(formula, data = local_data)
  }
  m_local <- fit_local(f)
  expect_error(
    check_outliers(m_local, group = "Subject"),
    "(`local_data`) could not be found",
    fixed = TRUE
  )
})


test_that("check_outliers, group arguments and print details", {
  data(sleepstudy, package = "lme4")
  m <- lme4::lmer(Reaction ~ Days + (1 | Subject), sleepstudy, subset = Subject != "308")
  out <- check_outliers(m, group = "Subject", method = c("pareto", "cook"))
  expect_identical(levels(as.data.frame(out)$Group), setdiff(levels(sleepstudy$Subject), "308"))
  expect_error(
    check_outliers(m, group = "Subject", threshold = -1),
    "non-negative number",
    fixed = TRUE
  )
  expect_identical(
    attr(check_outliers(m, group = "Subject", threshold = c(cook = 0.1)), "threshold"),
    list(cook = 0.1)
  )

  # alerts for lme4 models do not name glmmTMB checks
  local_mocked_bindings(.update_model = function(x, data, start, weights = NULL, subset = NULL) {
    stop("forced refit error", call. = FALSE)
  })
  alert <- tryCatch(
    check_outliers(m, group = "Subject"),
    message = function(msg) gsub("\\s+", " ", conditionMessage(msg))
  )
  expect_match(alert, "could not be computed for any level", fixed = TRUE)
  expect_false(grepl("Hessian", alert, fixed = TRUE))
  expect_false(grepl("weights", alert, fixed = TRUE))
})


test_that("check_outliers, group print names levels without a distance", {
  data(sleepstudy, package = "lme4")
  m <- lme4::lmer(Reaction ~ Days + (1 | Subject), sleepstudy)
  update_model <- .update_model
  local_mocked_bindings(.update_model = function(x, data, start, weights = NULL, subset = NULL) {
    if ("308" %in% data$Subject[!subset]) {
      stop("forced refit error", call. = FALSE)
    }
    update_model(x, data = data, start = start, weights = weights, subset = subset)
  })
  out <- suppressMessages(check_outliers(m, group = "Subject", threshold = 100))
  expect_output(print(out), "OK: No outliers detected.", fixed = TRUE)
  expect_output(
    print(out),
    "Cook's distance could not be computed for 1 level, which is not counted as outlier.",
    fixed = TRUE
  )
})


test_that("check_outliers, group detects changed variables behind model terms", {
  # `Days` is only in the model frame as `scale(Days)`, so the columns of the
  # model frame cannot show the change. The refit that leaves nothing out can.
  data(sleepstudy, package = "lme4")
  d <- sleepstudy
  m <- lme4::lmer(Reaction ~ scale(Days) + (1 | Subject), d)
  d$Days <- d$Days^2
  expect_error(
    check_outliers(m, group = "Subject"),
    "a refit on this",
    fixed = TRUE
  )

  d <- sleepstudy
  m_tmb <- glmmTMB::glmmTMB(Reaction ~ scale(Days) + (1 | Subject), d)
  d$Days <- d$Days^2
  expect_error(
    check_outliers(m_tmb, group = "Subject"),
    "a refit on this",
    fixed = TRUE
  )
})
