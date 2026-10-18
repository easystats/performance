skip_on_cran()
skip_if_not_installed("see", minimum_version = "0.9.1")

d <- data.frame(
  y = c(
    101L,
    219L,
    70L,
    67L,
    31L,
    243L,
    41L,
    250L,
    140L,
    210L,
    22L,
    121L,
    282L,
    144L,
    223L,
    297L,
    20L,
    13L,
    148L,
    57L,
    143L,
    109L,
    273L,
    83L,
    28L,
    153L,
    261L,
    209L,
    134L,
    95L,
    17L,
    11L
  ),
  x = c(
    9.43240344971252,
    15.2821279871535,
    29.1169666338249,
    14.0859852649899,
    15.6464367574587,
    8.86321119888889,
    67.9427830082998,
    8.60340052656454,
    21.9029971533007,
    19.8586681899898,
    9.68086847417484,
    25.0990259928273,
    16.1398284119823,
    29.3829154122785,
    31.5592351678585,
    14.0320212214305,
    31.3815560884357,
    18.3566866719804,
    28.3822251897697,
    67.9433435559261,
    26.6656339570149,
    5.83151069454924,
    7.93210796912854,
    35.9198997836162,
    21.6330853399868,
    34.484392512508,
    35.9444483480784,
    28.4517728364097,
    27.2868577206239,
    8.1803022427107,
    46.3029232706936,
    41.516915503604
  ),
  offset = c(
    481L,
    515L,
    396L,
    451L,
    547L,
    409L,
    375L,
    430L,
    526L,
    456L,
    450L,
    425L,
    406L,
    396L,
    421L,
    417L,
    430L,
    419L,
    436L,
    517L,
    511L,
    435L,
    489L,
    417L,
    372L,
    373L,
    351L,
    367L,
    350L,
    339L,
    169L,
    63L
  )
)

suppressWarnings({
  m <- glm(y ~ x + offset(offset), family = poisson, data = d)
})

test_that("`check_model()` works if convergence issues", {
  x <- check_model(m, verbose = FALSE)
  expect_s3_class(x, "check_model")
  expect_warning(
    check_model(x = m, verbose = FALSE),
    regex = "Argument `x` is deprecated",
    fixed = TRUE
  )
})

test_that("`check_outliers()` works if convergence issues", {
  x <- check_outliers(m, verbose = FALSE)
  expect_s3_class(x, "check_outliers")
})

test_that("`check_model()` for invalid models", {
  dd <- data.frame(y = as.difftime(0:5, units = "days"))
  m1 <- lm(y ~ 1, data = dd)
  expect_message(check_model(m1), regex = "Date variables are not")
})

test_that("`check_model()` works for quantreg", {
  skip_if_not_installed("quantreg")
  data(engel, package = "quantreg")
  qm <- quantreg::rq(foodexp ~ income, data = engel)
  x <- check_model(qm, verbose = FALSE)
  expect_s3_class(x, "check_model")
})

test_that("`check_model()` includes random-effects QQ for split random terms", {
  skip_if_not_installed("lme4")
  data(sleepstudy, package = "lme4")

  model <- lme4::lmer(
    Reaction ~ Days + (1 | Subject) + (0 + Days | Subject),
    data = sleepstudy
  )

  out <- check_model(model, check = "reqq", verbose = FALSE)
  expect_true("REQQ" %in% names(out))
  expect_length(out$REQQ, 1)
  expect_s3_class(out$REQQ[[1]], "data.frame")
  expect_equal(nrow(out$REQQ[[1]]), 2 * nrow(lme4::ranef(model)$Subject))
  expect_setequal(as.character(unique(out$REQQ[[1]]$facet)), c("(Intercept)", "Days"))
})

test_that("`check_model()` respects user-supplied `show_ci`", {
  data(iris)
  data(mtcars)
  m_factor <- lm(Sepal.Length ~ Species, data = iris)
  m_numeric <- lm(mpg ~ wt, data = mtcars)

  # defaults: no CI for models with only categorical predictors
  expect_false(attr(check_model(m_factor), "show_ci"))
  expect_true(attr(check_model(m_numeric), "show_ci"))

  # user choice overrides the defaults
  expect_true(attr(check_model(m_factor, show_ci = TRUE), "show_ci"))
  expect_false(attr(check_model(m_numeric, show_ci = FALSE), "show_ci"))
})

test_that("`check_model()` stores `show_smooth`", {
  data(mtcars)
  models <- list(
    lm = lm(mpg ~ wt, data = mtcars),
    glm = glm(am ~ wt, family = binomial, data = mtcars)
  )
  # simulate_residuals() needs DHARMa
  if (requireNamespace("DHARMa", quietly = TRUE)) {
    models$simres <- simulate_residuals(lm(mpg ~ wt, data = mtcars))
  }
  for (m in models) {
    expect_true(attr(check_model(m), "show_smooth"))
    expect_false(attr(check_model(m, show_smooth = FALSE), "show_smooth"))
  }
})

test_that("`check_model()` forwards `type` and `size_title` for simulated residuals", {
  skip_if_not_installed("DHARMa")
  data(mtcars)
  m <- glm(am ~ wt, data = mtcars, family = binomial)
  p <- glm(carb ~ wt, data = mtcars, family = poisson)

  # the default for binary and count responses is used, as for the glm itself
  expect_identical(attr(check_model(simulate_residuals(m)), "type"), "discrete_interval")
  expect_identical(attr(check_model(simulate_residuals(p)), "type"), "discrete_interval")
  expect_identical(
    attr(check_model(DHARMa::simulateResiduals(m)), "type"),
    "discrete_interval"
  )

  # user values are kept
  expect_identical(
    attr(check_model(simulate_residuals(m), type = "density"), "type"),
    "density"
  )
  expect_identical(
    attr(check_model(simulate_residuals(m), size_title = 20), "title_size"),
    20
  )
})

test_that("`check_model()` forwards `type` and `size_title` for `model_fit` objects", {
  data(mtcars)
  m <- glm(am ~ wt, data = mtcars, family = binomial)
  # the class that parsnip gives its fits
  obj <- structure(list(fit = m), class = "model_fit")

  expect_identical(attr(check_model(obj), "type"), "discrete_interval")
  expect_identical(attr(check_model(obj, type = "density"), "type"), "density")
  expect_identical(attr(check_model(obj, size_title = 20), "title_size"), 20)
})

test_that("`check_model()` forwards `type`, `size_title`, and `detrend` for Stan models", {
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("DHARMa")
  skip_if_not_installed("curl")
  skip_if_not_installed("httr2")
  skip_if_offline()
  # bernoulli model of `vs ~ wt`; brms models use the same method
  m <- insight::download_model("stanreg_glm_1")
  skip_if(is.null(m))

  expect_identical(attr(check_model(m), "type"), "discrete_interval")
  expect_identical(attr(check_model(m, type = "density"), "type"), "density")
  expect_identical(attr(check_model(m, size_title = 20), "title_size"), 20)
  # simulated residuals are not detrended by default
  expect_false(attr(check_model(m), "detrend"))
  expect_true(attr(check_model(m, detrend = TRUE), "detrend"))
})

test_that("`check_model()` forwards `detrend` for wrapped models", {
  skip_if_not_installed("DHARMa")
  data(mtcars)
  m <- glm(am ~ wt, data = mtcars, family = binomial)
  l <- lm(mpg ~ wt, data = mtcars)

  # simulated residuals are not detrended by default
  expect_false(attr(check_model(simulate_residuals(m)), "detrend"))
  expect_false(attr(check_model(DHARMa::simulateResiduals(m)), "detrend"))
  expect_false(attr(check_model(simulate_residuals(l)), "detrend"))
  expect_false(attr(check_model(l, residual_type = "simulated"), "detrend"))

  # the defaults of the bare models, which `model_fit` objects now get too
  expect_false(attr(check_model(m), "detrend"))
  expect_true(attr(check_model(l), "detrend"))
  expect_false(attr(
    check_model(structure(list(fit = m), class = "model_fit")),
    "detrend"
  ))
  expect_true(attr(check_model(structure(list(fit = l), class = "model_fit")), "detrend"))

  # user values are kept
  expect_true(attr(check_model(simulate_residuals(m), detrend = TRUE), "detrend"))
  expect_false(attr(
    check_model(structure(list(fit = l), class = "model_fit"), detrend = FALSE),
    "detrend"
  ))
})

test_that("`check_model()` keeps the defaults for wrapped linear models", {
  skip_if_not_installed("DHARMa")
  data(mtcars)
  l <- lm(mpg ~ wt, data = mtcars)
  out <- list(
    model_fit = check_model(structure(list(fit = l), class = "model_fit")),
    simres = check_model(simulate_residuals(l))
  )
  for (x in out) {
    expect_identical(attr(x, "type"), "density")
    expect_identical(attr(x, "title_size"), 12)
  }
})

test_that("every `check_model()` method has a `show_smooth` argument", {
  methods <- ls(asNamespace("performance"), pattern = "^check_model\\.")
  expect_gt(length(methods), 0)
  for (fun in methods) {
    expect_true(
      "show_smooth" %in%
        names(formals(get(fun, envir = asNamespace("performance")))),
      info = fun
    )
  }
})

test_that("`check_model()` checks `show_smooth`", {
  data(mtcars)
  m <- lm(mpg ~ wt, data = mtcars)
  for (value in list(NA, 1, "no", c(TRUE, FALSE))) {
    expect_error(
      check_model(m, show_smooth = value),
      "`show_smooth` must be either `TRUE` or `FALSE`.",
      fixed = TRUE
    )
  }
})

test_that("`check_model()` warnings for tweedie", {
  skip_if_not_installed("glmmTMB")
  skip_if_not_installed("lme4")
  data(sleepstudy, package = "lme4")
  set.seed(123)
  d <- sleepstudy[sample.int(50), ]
  m <- suppressWarnings(glmmTMB::glmmTMB(
    Reaction ~ Days,
    data = d,
    family = glmmTMB::tweedie
  ))
  expect_message(
    expect_message(
      check_model(m, iterations = 2, verbose = TRUE),
      regex = "Not enough model terms"
    )
  )
})


test_that("`check_model()` warnings for zero-infl", {
  skip_if_not_installed("pscl")
  data(bioChemists, package = "pscl")
  model <- pscl::zeroinfl(
    art ~ fem + mar + kid5 + ment | kid5 + phd,
    data = bioChemists
  )
  expect_message(
    expect_message(check_model(model, verbose = TRUE), regex = "Cannot simulate"),
    regex = "Homogeneity"
  )
})


test_that("`check_model()` no warnings for quasipoisson", {
  skip_if_not_installed("datawizard")
  set.seed(250419)
  # Generate random x values
  x <- rnorm(
    n = 500,
    mean = 5,
    sd = 2
  )
  # Generate y values y = 5x + e
  y <- 5 *
    x +
    rnorm(
      n = 500,
      mean = 5,
      sd = 2
    )
  # Generate z as offset
  z <- runif(500, min = 0, max = 6719)
  mock_data <- data.frame(x, y, z) |>
    # both should be whole numbers since they're counts
    datawizard::data_modify(y = round(y), z = round(z)) |>
    datawizard::data_filter(x >= 0, y >= 0)
  # Run model
  model1 <- glm(y ~ x + offset(log(z)), family = "quasipoisson", data = mock_data)
  expect_message(check_model(model1, verbose = TRUE), regex = "Not enough")
  expect_silent(check_model(model1))
})


test_that("`check_model()` overdispersion data use the quasipoisson dispersion", {
  m_quasi <- glm(DriversKilled ~ PetrolPrice, family = quasipoisson, data = Seatbelts)
  phi <- summary(m_quasi)$dispersion
  out <- .model_diagnostic_overdispersion(m_quasi)
  expect_equal(out$V, phi * unname(stats::fitted(m_quasi)), tolerance = 1e-8)
  expect_equal(
    out$StdRes,
    unname(stats::residuals(m_quasi, type = "pearson")) / sqrt(phi),
    tolerance = 1e-8
  )

  # a saturated quasipoisson model has no dispersion estimate, so phi is 1
  m_sat <- glm(
    y ~ factor(x),
    family = quasipoisson,
    data = data.frame(y = c(2, 5, 9), x = 1:3)
  )
  out <- .model_diagnostic_overdispersion(m_sat)
  expect_equal(out$V, unname(stats::fitted(m_sat)), tolerance = 1e-8)
  expect_s3_class(check_model(m_sat, check = "overdispersion"), "check_model")

  # missing Pearson residuals leave out `StdRes` instead of failing
  local({
    local_mocked_bindings(get_residuals = function(...) NULL, .package = "insight")
    out <- .model_diagnostic_overdispersion(m_quasi)
    expect_null(out$StdRes)
    expect_equal(out$V, phi * unname(stats::fitted(m_quasi)), tolerance = 1e-8)
  })

  # the Poisson model keeps the Poisson variance
  m_pois <- glm(DriversKilled ~ PetrolPrice, family = poisson, data = Seatbelts)
  out <- .model_diagnostic_overdispersion(m_pois)
  expect_equal(out$V, unname(stats::fitted(m_pois)), tolerance = 1e-8)
  expect_equal(
    out$StdRes,
    unname(stats::residuals(m_pois, type = "pearson")),
    tolerance = 1e-8
  )
})


test_that("`check_model()` with transformed response when named as function", {
  data(mtcars)
  # rename variable, so it equals to a valid R function
  mtcars$rt <- mtcars$mpg
  model <- lm(log(rt) ~ disp, data = mtcars)
  out <- check_predictions(model)
  expect_s3_class(out, "performance_pp_check")
})


# brms models that cannot be converted to frequentist models ----------------

skip_if_brms_fixture_unavailable <- function() {
  skip_if_not_installed("brms")
  skip_if_not_installed("bayestestR", minimum_version = "0.20.0")
  skip_if_not_installed("modelbased")
  skip_if_not_installed("curl")
  skip_if_not_installed("httr2")
  skip_if_offline()
}

mock_failed_conversion <- function(env = parent.frame()) {
  testthat::local_mocked_bindings(
    bayesian_as_frequentist = function(...) stop("conversion failed", call. = FALSE),
    .package = "bayestestR",
    .env = env
  )
}

test_that("`check_model()` falls back to Bayesian checks if brms conversion fails", {
  skip_if_brms_fixture_unavailable()
  m <- insight::download_model("brms_1")
  skip_if(is.null(m))
  mock_failed_conversion()

  out <- suppressMessages(check_model(m))
  expect_s3_class(out, "check_model")
  expect_named(out, c("VIF", "PP_CHECK"), ignore.order = TRUE)
  expect_true(all(c("x", "y", "group") %in% colnames(out$VIF)))
  expect_s3_class(out$PP_CHECK, "performance_pp_check")
  expect_identical(attr(out, "check"), "all")
  expect_identical(attr(out, "model_class"), "brmsfit")
  expect_false(is.null(attr(out, "model_info")))
  expect_true(attr(out, "show_smooth"))

  out <- suppressMessages(check_model(m, check = "vif", show_smooth = FALSE))
  expect_named(out, "VIF")
  expect_false(attr(out, "show_smooth"))
})

test_that("`check_model()` fallback for brms models can be plotted", {
  skip_if_brms_fixture_unavailable()
  m <- insight::download_model("brms_1")
  skip_if(is.null(m))
  mock_failed_conversion()

  out <- suppressMessages(check_model(m))
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_no_error(print(plot(out)))
})

test_that("`check_model()` alerts about skipped checks if brms conversion fails", {
  skip_if_brms_fixture_unavailable()
  m <- insight::download_model("brms_1")
  skip_if(is.null(m))
  mock_failed_conversion()

  for (verbose in c(FALSE, TRUE)) {
    msgs <- capture_messages(check_model(m, verbose = verbose))
    msgs <- gsub("\\s+", " ", msgs)
    alert <- msgs[grepl("conversion to a frequentist model failed", msgs, fixed = TRUE)]
    expect_length(alert, 1)
    for (panel in c("QQ", "NORM", "NCV", "HOMOGENEITY", "OUTLIERS", "INFLUENTIAL")) {
      expect_match(alert, paste0("\\b", panel, "\\b"))
    }
    # panels that do not apply to a non-mixed gaussian model are not listed
    expect_no_match(alert, "REQQ|BINNED_RESID|OVERDISPERSION")
    # the alert includes the error from the failed conversion
    expect_match(
      alert,
      "Reason for the failed conversion: conversion failed",
      fixed = TRUE
    )
  }
})

test_that("`check_model()` lets conversion errors propagate with `easystats_errors`", {
  skip_if_brms_fixture_unavailable()
  m <- insight::download_model("brms_1")
  skip_if(is.null(m))
  mock_failed_conversion()
  withr::local_options(easystats_errors = TRUE)

  expect_error(check_model(m), "conversion failed", fixed = TRUE)
})

test_that("`check_model()` errors if no check works after brms conversion fails", {
  skip_if_brms_fixture_unavailable()
  m <- insight::download_model("brms_1")
  skip_if(is.null(m))
  mock_failed_conversion()

  expect_error(
    check_model(m, check = c("qq", "normality")),
    "none\\s+of\\s+the\\s+requested\\s+checks\\s+could\\s+be\\s+computed"
  )
})

test_that("`check_model()` needs one `response` for multivariate brms models", {
  # `response` is a formal argument, so `resp = ` would also partial-match it
  expect_true(
    "response" %in% names(formals(utils::getS3method("check_model", "brmsfit")))
  )
  skip_if_brms_fixture_unavailable()
  m <- insight::download_model("brms_mv_1")
  skip_if(is.null(m))
  called <- FALSE
  testthat::local_mocked_bindings(
    bayesian_as_frequentist = function(...) {
      called <<- TRUE
      stop("conversion failed", call. = FALSE)
    },
    .package = "bayestestR"
  )

  for (response in list(NULL, "mpg", c("cyl", "gear"))) {
    err <- expect_error(
      check_model(m, response = response),
      "more\\s+than\\s+one\\s+response"
    )
    msg <- gsub("\\s+", " ", conditionMessage(err))
    for (name in c("cyl", "gear", "carb", "response")) {
      expect_match(msg, paste0("`", name, "`"), fixed = TRUE)
    }
    expect_no_match(msg, "`resp`", fixed = TRUE)
  }
  # `resp` next to `response` would otherwise be dropped silently
  expect_error(
    check_model(m, response = "gear", resp = "carb"),
    "Please use only the `response` argument",
    fixed = TRUE
  )
  expect_false(called)
  # the mock is in reach of `bayestestR::`, so `called` can become TRUE
  try(bayestestR::bayesian_as_frequentist(m), silent = TRUE)
  expect_true(called)
})

test_that("`check_model()` checks one response of a multivariate brms model", {
  skip_if_brms_fixture_unavailable()
  m <- insight::download_model("brms_mv_1")
  skip_if(is.null(m))

  out <- check_model(m, response = "gear")
  expect_s3_class(out, "check_model")
  expect_true(setequal(
    names(out),
    c("VIF", "QQ", "NORM", "NCV", "HOMOGENEITY", "OUTLIERS", "INFLUENTIAL", "PP_CHECK")
  ))
  d <- insight::get_data(m)
  expect_equal(out$PP_CHECK$y, d$gear, ignore_attr = TRUE)
  expected_fitted <- stats::fitted(stats::lm(gear ~ wt + hp, data = d))
  expect_lt(max(abs(out$NCV$x - expected_fitted)), 1e-8)
})

test_that("`check_model()` falls back to Bayesian checks for one response of a multivariate brms model", {
  skip_if_brms_fixture_unavailable()
  m <- insight::download_model("brms_mv_1")
  skip_if(is.null(m))
  received <- "not called"
  received_names <- NULL
  testthat::local_mocked_bindings(
    bayesian_as_frequentist = function(...) {
      received <<- list(...)[["resp"]]
      received_names <<- names(list(...))
      stop("conversion failed", call. = FALSE)
    },
    .package = "bayestestR"
  )

  msgs <- capture_messages({
    out <- check_model(m, response = "gear")
  })
  # a conversion function without a `response` argument gets `resp`
  expect_identical(received, "gear")
  expect_false("response" %in% received_names)
  expect_named(out, "PP_CHECK")
  expect_equal(out$PP_CHECK$y, insight::get_data(m)$gear, ignore_attr = TRUE)
  msgs <- gsub("\\s+", " ", paste(msgs, collapse = " "))
  skipped <- regmatches(msgs, regexec("Skipped checks: ([^.]*)\\.", msgs))[[1]][2]
  expect_true(setequal(
    strsplit(skipped, ", ", fixed = TRUE)[[1]],
    c("QQ", "NORM", "NCV", "HOMOGENEITY", "OUTLIERS", "INFLUENTIAL")
  ))
  expect_true(attr(out, "model_info")$is_linear)
})

test_that("`check_model()` names the response with the argument of `bayesian_as_frequentist()`", {
  skip_if_brms_fixture_unavailable()
  m <- insight::download_model("brms_mv_1")
  skip_if(is.null(m))
  received <- NULL
  # `sys.call()` keeps the argument names as written, so a partial match of
  # `resp = ` to `response` is not hidden as `match.call()` would hide it
  mocks <- list(
    response = function(model, data = NULL, REML = TRUE, response = NULL) {
      received <<- list(names = names(sys.call()), value = response)
      stop("conversion failed", call. = FALSE)
    },
    resp = function(model, data = NULL, REML = TRUE, resp = NULL) {
      received <<- list(names = names(sys.call()), value = resp)
      stop("conversion failed", call. = FALSE)
    }
  )

  for (arg in names(mocks)) {
    received <- NULL
    testthat::with_mocked_bindings(
      suppressMessages(check_model(m, response = "gear")),
      bayesian_as_frequentist = mocks[[arg]],
      .package = "bayestestR"
    )
    other <- setdiff(names(mocks), arg)
    expect_true(arg %in% received$names, label = arg)
    expect_false(other %in% received$names, label = arg)
    expect_identical(received$value, "gear", label = arg)
  }
})

test_that("`check_model()` converts multivariate brms models without a partial argument match", {
  skip_if_brms_fixture_unavailable()
  skip_if_not(
    "response" %in% names(formals(bayestestR::bayesian_as_frequentist)),
    "bayestestR::bayesian_as_frequentist() has no `response` argument"
  )
  m <- insight::download_model("brms_mv_1")
  skip_if(is.null(m))
  withr::local_options(warnPartialMatchArgs = TRUE)

  warnings <- character()
  out <- withCallingHandlers(
    check_model(m, response = "gear"),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      # other warnings still reach the test output
      if (grepl("partial argument match", conditionMessage(w), fixed = TRUE)) {
        invokeRestart("muffleWarning")
      }
    }
  )
  # the conversion worked, so the frequentist panels are there
  expect_true(all(c("VIF", "NCV", "PP_CHECK") %in% names(out)))
  expect_false(any(grepl("'resp' to 'response'", warnings, fixed = TRUE)))
})

test_that("`check_model()` keeps all panels for brms models that can be converted", {
  skip_if_brms_fixture_unavailable()
  m <- insight::download_model("brms_1")
  skip_if(is.null(m))

  out <- check_model(m)
  expect_true(setequal(
    names(out),
    c("VIF", "QQ", "NORM", "NCV", "HOMOGENEITY", "OUTLIERS", "INFLUENTIAL", "PP_CHECK")
  ))
})
