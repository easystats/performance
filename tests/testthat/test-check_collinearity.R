# https://github.com/easystats/performance/pull/547
test_that("check_collinearity, correct order in print", {
  data(mtcars)
  m <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
  out <- capture.output(print(check_collinearity(m, verbose = FALSE)))
  expect_identical(
    out,
    c(
      "# Check for Multicollinearity",
      "",
      "Low Correlation",
      "",
      " Term  VIF    VIF 95% CI adj. VIF Tolerance Tolerance 95% CI",
      " gear 1.53 [1.19,  2.51]     1.24      0.65     [0.40, 0.84]",
      "",
      "Moderate Correlation",
      "",
      " Term  VIF    VIF 95% CI adj. VIF Tolerance Tolerance 95% CI",
      "   wt 5.05 [3.21,  8.41]     2.25      0.20     [0.12, 0.31]",
      "  cyl 5.41 [3.42,  9.04]     2.33      0.18     [0.11, 0.29]",
      " disp 9.97 [6.08, 16.85]     3.16      0.10     [0.06, 0.16]"
    )
  )
})


test_that("check_collinearity, interaction", {
  m <- lm(mpg ~ wt * cyl, data = mtcars)
  expect_message(check_collinearity(m), regex = "Model has interaction terms")
})


test_that("check_collinearity, brms models keep interaction terms", {
  skip_on_cran()
  skip_if_not_installed("brms")
  skip_if_not_installed("curl")
  skip_if_offline()
  skip_if_not_installed("httr2")

  # r | trials(n) ~ treat * c2, where treat is a factor
  m <- insight::download_model("brms_2")
  skip_if(is.null(m))

  out <- check_collinearity(m, verbose = FALSE)
  expect_identical(out$Term, c("treat", "c2", "treat:c2"))

  # each term has one coefficient, so its VIF is the diagonal of the inverse
  # correlation matrix of the slope coefficients
  R <- stats::cov2cor(stats::vcov(m)[-1, -1])
  expect_equal(out$VIF, unname(diag(solve(R))), tolerance = 1e-6)
})


test_that("check_collinearity, brms models with more than one intercept", {
  skip_on_cran()
  skip_if_not_installed("brms")
  skip_if_not_installed("curl")
  skip_if_offline()
  skip_if_not_installed("httr2")

  # cumulative model rating ~ treat + period + carry, with three intercepts
  m <- insight::download_model("brms_ordinal_2")
  skip_if(is.null(m))

  out <- check_collinearity(m, verbose = FALSE)
  expect_identical(out$Term, c("treat", "period", "carry"))

  # the VIFs come from the slope coefficients only, without the intercepts
  v <- stats::vcov(m)
  slopes <- c("treat", "period", "carry")
  R <- stats::cov2cor(v[slopes, slopes])
  expect_equal(out$VIF, unname(diag(solve(R))), tolerance = 1e-6)
})


test_that("check_collinearity, fallback path keeps interactions", {
  m <- lm(mpg ~ wt * cyl, data = mtcars)
  m_no_int <- lm(mpg ~ 0 + wt * cyl, data = mtcars)
  mm <- stats::model.matrix(stats::terms(m), data = mtcars)
  mm_no_int <- stats::model.matrix(stats::terms(m_no_int), data = mtcars)
  params <- insight::clean_names(insight::find_parameters(m)[["conditional"]])
  params_no_int <- insight::clean_names(insight::find_parameters(m_no_int)[[
    "conditional"
  ]])
  coef_names <- insight::clean_names(colnames(mm))
  coef_names_no_int <- insight::clean_names(colnames(mm_no_int))
  assign <- attr(mm, "assign")
  assign_no_int <- attr(mm_no_int, "assign")
  if (!"intercept" %in% params && "intercept" %in% coef_names) {
    int_pos <- which(coef_names == "intercept")
    coef_names <- coef_names[-int_pos]
    assign <- assign[-int_pos]
  }
  if (!"intercept" %in% params_no_int && "intercept" %in% coef_names_no_int) {
    int_pos <- which(coef_names_no_int == "intercept")
    coef_names_no_int <- coef_names_no_int[-int_pos]
    assign_no_int <- assign_no_int[-int_pos]
  }
  expected <- assign[match(params, coef_names)]
  expected_no_int <- assign_no_int[match(params_no_int, coef_names_no_int)]

  testthat::local_mocked_bindings(
    get_modelmatrix = function(...) {
      structure(matrix(0, nrow = 1, ncol = 1), assign = NULL)
    },
    .package = "insight"
  )
  out <- performance:::.term_assignments(m, component = "conditional")
  out_no_int <- performance:::.term_assignments(m_no_int, component = "conditional")
  expect_identical(out, expected)
  expect_false(anyNA(out))
  expect_identical(out_no_int, expected_no_int)
  expect_false(anyNA(out_no_int))
})


test_that("check_collinearity, fallback returns NULL if matrix rebuild fails", {
  m <- lm(mpg ~ wt * cyl, data = mtcars)
  testthat::local_mocked_bindings(
    get_data = function(...) NULL,
    .package = "insight"
  )
  testthat::local_mocked_bindings(
    model.matrix = function(...) stop("rebuild failed"),
    .package = "stats"
  )
  expect_null(
    performance:::.find_term_assignment(m, component = "conditional")
  )
})


test_that("check_collinearity", {
  skip_if_not_installed("glmmTMB")
  skip_if_not(getRversion() >= "4.0.0")

  data(Salamanders, package = "glmmTMB")

  m1 <- glmmTMB::glmmTMB(
    count ~ spp + mined + (1 | site),
    ziformula = ~spp,
    Salamanders,
    family = poisson()
  )
  expect_equal(
    suppressWarnings(
      check_collinearity(m1, component = "conditional", verbose = FALSE)$VIF
    ),
    c(1.00037354840318, 1.00037354840318),
    tolerance = 1e-3
  )
  expect_equal(
    suppressWarnings(check_collinearity(m1, component = "all", verbose = FALSE)$VIF),
    c(1.00037354840318, 1.00037354840318),
    tolerance = 1e-3
  )
  expect_null(check_collinearity(m1, verbose = FALSE, component = "zero_inflated"))
})


test_that("check_collinearity", {
  skip_if_not_installed("glmmTMB")
  skip_if_not(getRversion() >= "4.0.0")

  data(Salamanders, package = "glmmTMB")

  m2 <- glmmTMB::glmmTMB(
    count ~ spp + mined + cover + (1 | site),
    ziformula = ~ spp + mined + cover,
    family = glmmTMB::nbinom2,
    data = Salamanders
  )

  expect_equal(
    suppressWarnings(
      check_collinearity(m2, component = "conditional", verbose = FALSE)$VIF
    ),
    c(1.09015, 1.2343, 1.17832),
    tolerance = 1e-3
  )
  expect_equal(
    suppressWarnings(
      check_collinearity(m2, component = "conditional", verbose = FALSE)$VIF_CI_low
    ),
    c(1.03392, 1.14674, 1.10105),
    tolerance = 1e-3
  )
  expect_equal(
    suppressWarnings(check_collinearity(m2, component = "all", verbose = FALSE)$VIF),
    c(1.09015, 1.2343, 1.17832, 1.26914, 1, 1.26914),
    tolerance = 1e-3
  )
  expect_equal(
    suppressWarnings(
      check_collinearity(m2, component = "all", verbose = FALSE)$VIF_CI_low
    ),
    c(1.03392, 1.14674, 1.10105, 1.17565, 1, 1.17565),
    tolerance = 1e-3
  )
  expect_equal(
    suppressWarnings(
      check_collinearity(m2, component = "zero_inflated", verbose = FALSE)$VIF
    ),
    c(1.26914, 1, 1.26914),
    tolerance = 1e-3
  )
  expect_equal(
    suppressWarnings(
      check_collinearity(
        m2,
        component = "zero_inflated",
        verbose = FALSE
      )$Tolerance_CI_high
    ),
    c(0.85059, 1, 0.85059),
    tolerance = 1e-3
  )

  suppressWarnings(coll <- check_collinearity(m2, component = "all", verbose = FALSE)) # nolint
  expect_true(all(coll$Tolerance < coll$Tolerance_CI_high))
  expect_true(all(coll$VIF > coll$VIF_CI_low))

  expect_identical(
    attributes(coll)$data$Component,
    c(
      "conditional",
      "conditional",
      "conditional",
      "zero inflated",
      "zero inflated",
      "zero inflated"
    )
  )
  expect_identical(
    colnames(attributes(coll)$CI),
    c("VIF_CI_low", "VIF_CI_high", "Tolerance_CI_low", "Tolerance_CI_high", "Component")
  )
})

test_that("check_collinearity | afex", {
  skip_on_cran()
  skip_if_not_installed("afex", minimum_version = "1.0.0")

  data(obk.long, package = "afex")

  obk.long$treatment <- as.character(obk.long$treatment)
  suppressWarnings(suppressMessages({
    aM <- afex::aov_car(
      value ~ treatment * gender + Error(id / (phase * hour)),
      data = obk.long
    )

    aW <- afex::aov_car(value ~ Error(id / (phase * hour)), data = obk.long)

    aB <- afex::aov_car(value ~ treatment * gender + Error(id), data = obk.long)
  }))

  expect_message(ccoM <- check_collinearity(aM)) # nolint
  expect_warning(expect_message(ccoW <- check_collinearity(aW))) # nolint
  expect_message(ccoB <- check_collinearity(aB), regexp = NA) # nolint

  expect_identical(nrow(ccoM), 15L)
  expect_identical(nrow(ccoW), 3L)
  expect_identical(nrow(ccoB), 3L)

  suppressWarnings(suppressMessages({
    aM <- afex::aov_car(
      value ~ treatment * gender + Error(id / (phase * hour)),
      include_aov = TRUE,
      data = obk.long
    )

    aW <- afex::aov_car(
      value ~ Error(id / (phase * hour)),
      include_aov = TRUE,
      data = obk.long
    )

    aB <- afex::aov_car(
      value ~ treatment * gender + Error(id),
      include_aov = TRUE,
      data = obk.long
    )
  }))

  expect_message(ccoM <- check_collinearity(aM)) # nolint
  expect_warning(expect_message(ccoW <- check_collinearity(aW))) # nolint
  expect_message(ccoB <- check_collinearity(aB), regexp = NA) # nolint

  expect_identical(nrow(ccoM), 15L)
  expect_identical(nrow(ccoW), 3L)
  expect_identical(nrow(ccoB), 3L)
})

test_that("check_collinearity, ci = NULL", {
  # 518
  data(npk)
  m <- lm(yield ~ N + P + K, npk)
  out <- check_collinearity(m, ci = NULL)

  # fmt: skip
  expect_identical(
    colnames(out),
    c(
      "Term", "VIF", "VIF_CI_low", "VIF_CI_high", "SE_factor", "Tolerance",
      "Tolerance_CI_low", "Tolerance_CI_high"
    )
  )
  expect_identical(
    capture.output(print(out)),
    c(
      "# Check for Multicollinearity",
      "",
      "Low Correlation",
      "",
      " Term VIF adj. VIF Tolerance",
      "    N   1        1         1",
      "    P   1        1         1",
      "    K   1        1         1"
    )
  )
})

test_that("check_collinearity, ci are NA", {
  skip_if_not_installed("fixest")
  data(mtcars)
  i <- fixest::i
  m_vif <- fixest::feols(mpg ~ disp + hp + wt + i(cyl) | carb, data = mtcars)
  out <- suppressWarnings(check_collinearity(m_vif))
  # fmt: skip
  expect_identical(
    colnames(out),
    c(
      "Term", "VIF", "VIF_CI_low", "VIF_CI_high", "SE_factor", "Tolerance",
      "Tolerance_CI_low", "Tolerance_CI_high"
    )
  )
})

test_that("check_collinearity, hurdle/zi models w/o zi-formula", {
  skip_if_not_installed("pscl")
  data("bioChemists", package = "pscl")
  m <- pscl::hurdle(
    art ~ fem + mar,
    data = bioChemists,
    dist = "poisson",
    zero.dist = "binomial",
    link = "logit"
  )
  out <- check_collinearity(m)
  # fmt: skip
  expect_named(
    out,
    c(
      "Term", "VIF", "VIF_CI_low", "VIF_CI_high", "SE_factor", "Tolerance",
      "Tolerance_CI_low", "Tolerance_CI_high", "Component"
    )
  )
  expect_equal(out$VIF, c(1.05772, 1.05772, 1.06587, 1.06587), tolerance = 1e-4)
  expect_snapshot(print(out))
})

test_that("check_collinearity, invalid data", {
  dd <- data.frame(y = as.difftime(0:5, units = "days"))
  m1 <- lm(y ~ 1, data = dd)
  expect_message(check_collinearity(m1), "Could not extract the variance-covariance")
})

test_that("check_collinearity, glmmTMB hurdle w/o zi", {
  skip_if_not_installed("glmmTMB")
  data(Salamanders, package = "glmmTMB")
  mod_trunc_error <- glmmTMB::glmmTMB(
    count ~ spp + mined + (1 | site),
    data = Salamanders[Salamanders$count > 0, , drop = FALSE],
    family = glmmTMB::truncated_nbinom2(),
    ziformula = ~0,
    dispformula = ~1
  )
  out <- check_collinearity(mod_trunc_error)
  expect_equal(out$VIF, c(1.03414, 1.03414), tolerance = 1e-3)
})

test_that("check_collinearity, validate adjusted vif against car", {
  skip_if_not_installed("car")
  data(mtcars)
  mod <- lm(mpg ~ cyl + hp + am, data = mtcars)
  out1 <- car::vif(mod)
  out2 <- check_collinearity(mod)
  expect_equal(out1, out2$VIF, tolerance = 1e-3, ignore_attr = TRUE)

  mod <- lm(mpg ~ as.factor(cyl) + hp + gear, data = mtcars)
  out1 <- car::vif(mod)
  out2 <- check_collinearity(mod)
  expect_equal(out1[, 1], out2$VIF, tolerance = 1e-3, ignore_attr = TRUE)
  expect_equal(out1[, 3], out2$SE_factor, tolerance = 1e-3, ignore_attr = TRUE)

  mtcars$gear <- factor(mtcars$gear)
  mod <- lm(mpg ~ as.factor(cyl) + hp + gear, data = mtcars)
  out1 <- car::vif(mod)
  out2 <- check_collinearity(mod)
  expect_equal(out1[, 1], out2$VIF, tolerance = 1e-3, ignore_attr = TRUE)
  expect_equal(out1[, 3], out2$SE_factor, tolerance = 1e-3, ignore_attr = TRUE)
})

test_that("check_collinearity, ordinal clmm models", {
  skip_if_not_installed("ordinal")
  set.seed(999)
  n <- 500
  x_continuous <- rnorm(n, mean = 0, sd = 1)
  x_binary <- sample(c(-0.5, 0.5), size = n, replace = TRUE, prob = c(0.85, 0.15))
  subject_id <- factor(rep(1:50, each = 10))
  random_intercepts <- rnorm(50, 0, 1)
  latent_y <- 2 *
    x_continuous +
    3 * x_binary +
    random_intercepts[as.numeric(subject_id)] +
    rlogis(n)
  y_ordinal <- cut(
    latent_y,
    breaks = 15,
    ordered_result = TRUE
  )
  dat <- data.frame(y_ordinal, x_continuous, x_binary, subject_id)
  mod_clmm <- ordinal::clmm(
    y_ordinal ~ x_continuous + x_binary + (1 | subject_id),
    data = dat
  )
  out <- check_collinearity(mod_clmm)
  expect_s3_class(out, "check_collinearity")
  expect_identical(out$Term, c("x_continuous", "x_binary"))
  expect_equal(out$VIF, c(1.12, 1.12), tolerance = 0.05)
})

test_that("check_collinearity, ordinal clm models", {
  skip_if_not_installed("ordinal")
  set.seed(999)
  n <- 500
  x_continuous <- rnorm(n, mean = 0, sd = 1)
  x_binary <- sample(c(-0.5, 0.5), size = n, replace = TRUE, prob = c(0.85, 0.15))
  latent_y <- 2 * x_continuous + 3 * x_binary + rlogis(n)
  y_ordinal <- cut(
    latent_y,
    breaks = 15,
    ordered_result = TRUE
  )
  dat <- data.frame(y_ordinal, x_continuous, x_binary)
  mod_clm <- ordinal::clm(
    y_ordinal ~ x_continuous + x_binary,
    data = dat
  )
  out <- check_collinearity(mod_clm)
  expect_s3_class(out, "check_collinearity")
  expect_identical(out$Term, c("x_continuous", "x_binary"))
  expect_equal(out$VIF, c(1.11, 1.11), tolerance = 0.05)
})

test_that("check_collinearity, ordinal clmm models with offset", {
  skip_if_not_installed("ordinal")
  set.seed(999)
  n <- 500
  x_continuous <- rnorm(n, mean = 0, sd = 1)
  x_binary <- sample(c(-0.5, 0.5), size = n, replace = TRUE, prob = c(0.85, 0.15))
  x_offset <- rnorm(n, mean = 0, sd = 0.5)
  subject_id <- factor(rep(1:50, each = 10))
  random_intercepts <- rnorm(50, 0, 1)

  latent_y <- 2 *
    x_continuous +
    3 * x_binary +
    random_intercepts[as.numeric(subject_id)] +
    x_offset +
    rlogis(n)
  y_ordinal <- cut(latent_y, breaks = 15, ordered_result = TRUE)
  dat <- data.frame(y_ordinal, x_continuous, x_binary, x_offset, subject_id)
  mod_clmm_offset <- ordinal::clmm(
    y_ordinal ~ x_continuous + x_binary + offset(x_offset) + (1 | subject_id),
    data = dat
  )
  out <- check_collinearity(mod_clmm_offset)
  expect_s3_class(out, "check_collinearity")
  expect_identical(out$Term, c("x_continuous", "x_binary"))
  expect_equal(out$VIF, c(1.12, 1.12), tolerance = 0.05)
})

test_that("check_collinearity, ordinal clm models with offset", {
  skip_if_not_installed("ordinal")
  set.seed(999)
  n <- 500
  x_continuous <- rnorm(n, mean = 0, sd = 1)
  x_binary <- sample(c(-0.5, 0.5), size = n, replace = TRUE, prob = c(0.85, 0.15))
  x_offset <- rnorm(n, mean = 0, sd = 0.5)
  latent_y <- 2 * x_continuous + 3 * x_binary + x_offset + rlogis(n)
  y_ordinal <- cut(latent_y, breaks = 15, ordered_result = TRUE)
  dat <- data.frame(y_ordinal, x_continuous, x_binary, x_offset)
  mod_clm_offset <- ordinal::clm(
    y_ordinal ~ x_continuous + x_binary + offset(x_offset),
    data = dat
  )
  out <- check_collinearity(mod_clm_offset)
  expect_s3_class(out, "check_collinearity")
  expect_identical(out$Term, c("x_continuous", "x_binary"))
  expect_equal(out$VIF, c(1.11, 1.11), tolerance = 0.05)
})

test_that("check_collinearity, standard lm models with offset", {
  # Standard linear model with an offset
  m_lm_offset <- lm(mpg ~ wt + cyl + offset(disp), data = mtcars)
  out <- check_collinearity(m_lm_offset)
  expect_s3_class(out, "check_collinearity")
  # The offset should not be evaluated for collinearity
  expect_identical(out$Term, c("wt", "cyl"))
  expect_false("disp" %in% out$Term)
})


test_that("check_collinearity, rank deficient.", {
  data(mtcars)
  mtcars$Z <- mtcars$wt + mtcars$cyl

  m1 <- lm(mpg ~ wt + cyl + Z, data = mtcars)
  m2 <- lm(mpg ~ wt + cyl, data = mtcars)
  expect_warning(
    {
      out1 <- check_collinearity(m1)
    },
    "Model matrix is rank deficient"
  )
  expect_silent({
    out2 <- check_collinearity(m2)
  })
  expect_identical(out1$VIF, out2$VIF)
})


test_that("check_collinearity, brms with 0 + Intercept syntax", {
  skip_on_cran()
  skip_if_not_installed("brms")
  skip_if_not_installed("BH")
  m <- suppressMessages(suppressWarnings(brms::brm(
    mpg ~ 0 + Intercept + cyl + disp + hp,
    data = mtcars,
    chains = 1,
    iter = 500,
    refresh = 0,
    seed = 123,
    silent = 2
  )))
  expect_silent({
    out <- check_collinearity(m, ci = NULL)
  })
  expect_identical(out$Term, c("cyl", "disp", "hp"))
  # VIFs for slopes, computed from the posterior correlation matrix
  v <- insight::get_varcov(m)
  R <- stats::cov2cor(v[-1, -1])
  expect_equal(out$VIF, unname(diag(solve(R))), tolerance = 1e-6)
})


# multinomial models ------------------------------------------------------

.multinom_data <- function(n = 300, levels = c("a", "b", "c")) {
  set.seed(123)
  d <- data.frame(x1 = rnorm(n), x3 = rnorm(n))
  d$x2 <- d$x1 + rnorm(n, sd = 0.3)
  d$f <- factor(sample(c("p", "q", "r"), n, replace = TRUE))
  eta <- sapply(seq_along(levels)[-1], function(k) {
    0.5 * k * d$x1 - 0.3 * d$x3 + 0.2 * (d$f == "q")
  })
  p <- cbind(1, exp(eta))
  p <- p / rowSums(p)
  d$y <- factor(
    apply(p, 1, function(i) sample(levels, 1, prob = i)),
    levels = levels
  )
  d
}

# generalized VIF of each term, from the variance-covariance matrix without
# intercepts; `terms` maps each coefficient name (without the outcome-level
# prefix) to its model term
.gvif_by_term <- function(v, outcome_levels, sep, terms) {
  coef_names <- sub(
    paste0("^(", paste(outcome_levels, collapse = "|"), ")", sep),
    "",
    colnames(v)
  )
  keep <- coef_names != "(Intercept)"
  cor_mat <- stats::cov2cor(v[keep, keep])
  coef_terms <- terms[coef_names[keep]]
  vapply(
    unique(terms),
    function(i) {
      idx <- which(coef_terms == i)
      det(cor_mat[idx, idx, drop = FALSE]) *
        det(cor_mat[-idx, -idx, drop = FALSE]) /
        det(cor_mat)
    },
    numeric(1)
  )
}

.multinom_terms <- c(
  x1 = "x1",
  x3 = "x3",
  x2 = "x2",
  fq = "f",
  fr = "f",
  `x1:x3` = "x1:x3",
  `x1:fq` = "x1:f",
  `x1:fr` = "x1:f"
)

test_that("check_collinearity, nnet::multinom with three outcome levels", {
  skip_if_not_installed("nnet")
  d <- .multinom_data()
  m <- nnet::multinom(y ~ x1 * x3 + x2 + x1 * f, data = d, trace = FALSE)
  expect_no_message(
    check_collinearity(m),
    message = "Model without intercept"
  )
  out <- check_collinearity(m)
  expect_identical(out$Term, c("x1", "x3", "x2", "f", "x1:x3", "x1:f"))
  expected <- .gvif_by_term(stats::vcov(m), c("b", "c"), ":", .multinom_terms)
  expect_equal(out$VIF, unname(expected), tolerance = 1e-6)
  # adjusted VIF uses the number of slope coefficients of each term across
  # all outcome equations
  m_coef <- c(x1 = 2, x3 = 2, x2 = 2, f = 4, `x1:x3` = 2, `x1:f` = 4)
  expect_equal(out$SE_factor, unname(out$VIF^(1 / (2 * m_coef))), tolerance = 1e-6)

  # same VIFs for another reference level of the outcome
  d$y <- stats::relevel(d$y, ref = "c")
  m2 <- nnet::multinom(y ~ x1 * x3 + x2 + x1 * f, data = d, trace = FALSE)
  out2 <- check_collinearity(m2)
  expect_identical(out2$Term, out$Term)
  expect_equal(out2$VIF, out$VIF, tolerance = 1e-4)
})

test_that("check_collinearity, mclogit::mblogit with three outcome levels", {
  skip_if_not_installed("nnet")
  skip_if_not_installed("mclogit")
  d <- .multinom_data()
  m <- suppressWarnings(
    mclogit::mblogit(y ~ x1 * x3 + x2 + x1 * f, data = d, trace = FALSE)
  )
  out <- check_collinearity(m)
  expect_identical(out$Term, c("x1", "x3", "x2", "f", "x1:x3", "x1:f"))
  expected <- .gvif_by_term(stats::vcov(m), c("b", "c"), "~", .multinom_terms)
  expect_equal(out$VIF, unname(expected), tolerance = 1e-6)
  m_coef <- c(x1 = 2, x3 = 2, x2 = 2, f = 4, `x1:x3` = 2, `x1:f` = 4)
  expect_equal(out$SE_factor, unname(out$VIF^(1 / (2 * m_coef))), tolerance = 1e-6)

  # agrees with nnet::multinom on the same data
  m2 <- nnet::multinom(y ~ x1 * x3 + x2 + x1 * f, data = d, trace = FALSE)
  out2 <- check_collinearity(m2)
  expect_equal(out$VIF, out2$VIF, tolerance = 1e-3)
})

test_that("check_collinearity, nnet::multinom with two outcome levels", {
  skip_if_not_installed("nnet")
  d <- .multinom_data(levels = c("a", "b"))
  m1 <- nnet::multinom(y ~ x1 + x2 + x3, data = d, trace = FALSE)
  m2 <- stats::glm(y ~ x1 + x2 + x3, data = d, family = stats::binomial())
  out1 <- check_collinearity(m1)
  out2 <- check_collinearity(m2)
  expect_identical(out1$Term, out2$Term)
  expect_equal(out1$VIF, out2$VIF, tolerance = 1e-3)
})

test_that("check_collinearity, multinomial models with unusual names", {
  skip_if_not_installed("nnet")
  skip_if_not_installed("mclogit")
  d <- .multinom_data(levels = c("a", "b c", "d:e"))
  d$intercept_shift <- d$x3
  terms <- c(x1 = "x1", x2 = "x2", intercept_shift = "intercept_shift")
  m1 <- nnet::multinom(y ~ x1 + x2 + intercept_shift, data = d, trace = FALSE)
  out1 <- check_collinearity(m1)
  expect_identical(out1$Term, c("x1", "x2", "intercept_shift"))
  expected <- .gvif_by_term(stats::vcov(m1), c("b c", "d:e"), ":", terms)
  expect_equal(out1$VIF, unname(expected), tolerance = 1e-6)
  m2 <- suppressWarnings(
    mclogit::mblogit(y ~ x1 + x2 + intercept_shift, data = d, trace = FALSE)
  )
  out2 <- check_collinearity(m2)
  expect_identical(out2$Term, c("x1", "x2", "intercept_shift"))
  expected <- .gvif_by_term(stats::vcov(m2), c("b c", "d:e"), "~", terms)
  expect_equal(out2$VIF, unname(expected), tolerance = 1e-6)
})

test_that("check_collinearity, multinomial models with ambiguous names", {
  skip_if_not_installed("nnet")
  # "b" with "x1:x3" and "b:x1" with "x3" give the same coefficient name
  d <- .multinom_data(levels = c("a", "b", "b:x1"))
  m <- nnet::multinom(y ~ x1 * x3, data = d, trace = FALSE)
  expect_message(
    check_collinearity(m),
    "Could not extract model terms"
  )
  expect_null(suppressMessages(check_collinearity(m)))
})

# generalized VIF of each term, from the variance-covariance matrix; `map`
# maps the name of each slope column of `v` to its model term, and columns
# not in `map` (the intercepts) are dropped
.gvif_by_map <- function(v, map) {
  keep <- colnames(v) %in% names(map)
  cor_mat <- stats::cov2cor(v[keep, keep])
  coef_terms <- map[colnames(cor_mat)]
  vapply(
    unique(map),
    function(i) {
      idx <- which(coef_terms == i)
      det(cor_mat[idx, idx, drop = FALSE]) *
        det(cor_mat[-idx, -idx, drop = FALSE]) /
        det(cor_mat)
    },
    numeric(1)
  )
}

.slope_map <- function(prefixes, sep = ":", suffix = FALSE) {
  slopes <- c(x1 = "x1", x3 = "x3", x2 = "x2", fq = "f", fr = "f")
  if (is.null(prefixes)) {
    return(slopes)
  }
  out <- rep(slopes, each = length(prefixes))
  if (suffix) {
    names(out) <- paste(rep(names(slopes), each = length(prefixes)), prefixes, sep = sep)
  } else {
    names(out) <- paste(prefixes, rep(names(slopes), each = length(prefixes)), sep = sep)
  }
  out
}

test_that("check_collinearity, brglm2::brmultinom", {
  skip_if_not_installed("brglm2")
  d <- .multinom_data()
  m <- brglm2::brmultinom(y ~ x1 + x3 + x2 + f, data = d)
  out <- check_collinearity(m)
  expect_identical(out$Term, c("x1", "x3", "x2", "f"))
  expected <- .gvif_by_map(stats::vcov(m), .slope_map(c("b", "c")))
  expect_equal(out$VIF, unname(expected), tolerance = 1e-8)
  m_coef <- c(2, 2, 2, 4)
  expect_equal(out$SE_factor, out$VIF^(1 / (2 * m_coef)), tolerance = 1e-8)

  # two outcome levels
  d2 <- .multinom_data(levels = c("a", "b"))
  m2 <- brglm2::brmultinom(y ~ x1 + x3 + x2 + f, data = d2)
  out2 <- check_collinearity(m2)
  expect_identical(out2$Term, c("x1", "x3", "x2", "f"))
  expected2 <- .gvif_by_map(stats::vcov(m2), .slope_map("b"))
  expect_equal(out2$VIF, unname(expected2), tolerance = 1e-8)
  m_coef2 <- c(1, 1, 1, 2)
  expect_equal(out2$SE_factor, out2$VIF^(1 / (2 * m_coef2)), tolerance = 1e-8)
})

test_that("check_collinearity, brglm2::bracl", {
  skip_if_not_installed("brglm2")
  d <- .multinom_data()
  d$o <- ordered(d$y)
  m1 <- brglm2::bracl(o ~ x1 + x3 + x2 + f, data = d, parallel = FALSE)
  out1 <- check_collinearity(m1)
  expect_identical(out1$Term, c("x1", "x3", "x2", "f"))
  expected1 <- .gvif_by_map(stats::vcov(m1), .slope_map(c("a", "b")))
  expect_equal(out1$VIF, unname(expected1), tolerance = 1e-8)
  m_coef1 <- c(2, 2, 2, 4)
  expect_equal(out1$SE_factor, out1$VIF^(1 / (2 * m_coef1)), tolerance = 1e-8)

  # adjacent-category and baseline-category logits are reparametrizations of
  # the same model, so the VIFs agree with brglm2::brmultinom
  m_base <- brglm2::brmultinom(y ~ x1 + x3 + x2 + f, data = d)
  expect_equal(out1$VIF, check_collinearity(m_base)$VIF, tolerance = 1e-6)

  # parallel model, with one slope for each model matrix column
  m2 <- brglm2::bracl(o ~ x1 + x3 + x2 + f, data = d, parallel = TRUE)
  out2 <- check_collinearity(m2)
  expect_identical(out2$Term, c("x1", "x3", "x2", "f"))
  expected2 <- .gvif_by_map(stats::vcov(m2), .slope_map(NULL))
  expect_equal(out2$VIF, unname(expected2), tolerance = 1e-8)
  m_coef2 <- c(1, 1, 1, 2)
  expect_equal(out2$SE_factor, out2$VIF^(1 / (2 * m_coef2)), tolerance = 1e-8)
})

test_that("check_collinearity, VGAM::vglm multinomial", {
  skip_if_not_installed("VGAM")
  skip_if_not_installed("nnet")
  d <- .multinom_data()
  m <- VGAM::vglm(y ~ x1 + x3 + x2 + f, family = VGAM::multinomial, data = d)
  out <- check_collinearity(m)
  expect_identical(out$Term, c("x1", "x3", "x2", "f"))
  expected <- .gvif_by_map(stats::vcov(m), .slope_map(c("1", "2"), suffix = TRUE))
  expect_equal(out$VIF, unname(expected), tolerance = 1e-8)
  m_coef <- c(2, 2, 2, 4)
  expect_equal(out$SE_factor, out$VIF^(1 / (2 * m_coef)), tolerance = 1e-8)

  # agrees with nnet::multinom on the same data
  m_nnet <- nnet::multinom(y ~ x1 + x3 + x2 + f, data = d, trace = FALSE)
  expect_equal(out$VIF, check_collinearity(m_nnet)$VIF, tolerance = 1e-3)

  # two outcome levels
  d2 <- .multinom_data(levels = c("a", "b"))
  m2 <- VGAM::vglm(y ~ x1 + x3 + x2 + f, family = VGAM::multinomial, data = d2)
  out2 <- check_collinearity(m2)
  expect_identical(out2$Term, c("x1", "x3", "x2", "f"))
  expected2 <- .gvif_by_map(stats::vcov(m2), .slope_map(NULL))
  expect_equal(out2$VIF, unname(expected2), tolerance = 1e-8)
  m_coef2 <- c(1, 1, 1, 2)
  expect_equal(out2$SE_factor, out2$VIF^(1 / (2 * m_coef2)), tolerance = 1e-8)
})

test_that("check_collinearity, VGAM::vglm cumulative", {
  skip_if_not_installed("VGAM")
  d <- .multinom_data()
  # ordinal outcome from a latent variable, so that the non-parallel
  # cumulative model fits without warnings
  d$o <- cut(
    0.5 * d$x1 - 0.3 * d$x3 + 0.2 * (d$f == "q") + stats::rlogis(nrow(d)),
    c(-Inf, -0.5, 0.5, Inf),
    labels = c("a", "b", "c"),
    ordered_result = TRUE
  )
  m1 <- VGAM::vglm(
    o ~ x1 + x3 + x2 + f,
    family = VGAM::cumulative(parallel = FALSE),
    data = d
  )
  out1 <- check_collinearity(m1)
  expect_identical(out1$Term, c("x1", "x3", "x2", "f"))
  expected1 <- .gvif_by_map(stats::vcov(m1), .slope_map(c("1", "2"), suffix = TRUE))
  expect_equal(out1$VIF, unname(expected1), tolerance = 1e-8)
  m_coef1 <- c(2, 2, 2, 4)
  expect_equal(out1$SE_factor, out1$VIF^(1 / (2 * m_coef1)), tolerance = 1e-8)

  m2 <- VGAM::vglm(
    o ~ x1 + x3 + x2 + f,
    family = VGAM::cumulative(parallel = TRUE),
    data = d
  )
  out2 <- check_collinearity(m2)
  expect_identical(out2$Term, c("x1", "x3", "x2", "f"))
  expected2 <- .gvif_by_map(stats::vcov(m2), .slope_map(NULL))
  expect_equal(out2$VIF, unname(expected2), tolerance = 1e-8)
  m_coef2 <- c(1, 1, 1, 2)
  expect_equal(out2$SE_factor, out2$VIF^(1 / (2 * m_coef2)), tolerance = 1e-8)
})

test_that("check_collinearity, multinomial models without intercept", {
  skip_if_not_installed("nnet")
  skip_if_not_installed("mclogit")
  d <- .multinom_data()
  slopes <- c(x1 = "x1", x3 = "x3", x2 = "x2")
  m1 <- nnet::multinom(y ~ 0 + x1 + x3 + x2, data = d, trace = FALSE)
  expect_message(
    out1 <- check_collinearity(m1),
    "Model without intercept"
  )
  map1 <- stats::setNames(
    rep(slopes, each = 2),
    paste(c("b", "c"), rep(names(slopes), each = 2), sep = ":")
  )
  expect_identical(out1$Term, c("x1", "x3", "x2"))
  expect_equal(out1$VIF, unname(.gvif_by_map(stats::vcov(m1), map1)), tolerance = 1e-8)

  m2 <- suppressWarnings(
    mclogit::mblogit(y ~ 0 + x1 + x3 + x2, data = d, trace = FALSE)
  )
  expect_message(
    out2 <- check_collinearity(m2),
    "Model without intercept"
  )
  map2 <- stats::setNames(
    rep(slopes, each = 2),
    paste(c("b", "c"), rep(names(slopes), each = 2), sep = "~")
  )
  expect_identical(out2$Term, c("x1", "x3", "x2"))
  expect_equal(out2$VIF, unname(.gvif_by_map(stats::vcov(m2), map2)), tolerance = 1e-8)
})

test_that("check_collinearity, exactly collinear multinomial model", {
  skip_if_not_installed("nnet")
  d <- .multinom_data()
  d$x4 <- d$x1 + d$x3
  m <- nnet::multinom(y ~ x1 + x3 + x4 + x2, data = d, trace = FALSE)
  expect_warning(
    out <- check_collinearity(m),
    "singular"
  )
  expect_null(out)
  expect_null(check_collinearity(m, verbose = FALSE))

  # correlated but not exactly collinear predictors give no warning
  m2 <- nnet::multinom(y ~ x1 * x3 + x2 + x1 * f, data = d, trace = FALSE)
  expect_no_warning(check_collinearity(m2))
})

test_that("check_collinearity, strong correlation inside one term is not singular", {
  set.seed(123)
  d <- data.frame(x = runif(300, 500, 510), z = rnorm(300))
  d$y <- d$x + d$z + rnorm(300)
  d$cnt <- stats::rpois(300, exp(0.1 * (d$x - 505) + 0.2 * d$z))
  map <- c(
    `poly(x, 3, raw = TRUE)1` = "poly(x, 3, raw = TRUE)",
    `poly(x, 3, raw = TRUE)2` = "poly(x, 3, raw = TRUE)",
    `poly(x, 3, raw = TRUE)3` = "poly(x, 3, raw = TRUE)",
    z = "z"
  )
  m1 <- lm(y ~ poly(x, 3, raw = TRUE) + z, data = d)
  expect_no_warning(out1 <- check_collinearity(m1))
  expect_equal(out1$VIF, unname(.gvif_by_map(stats::vcov(m1), map)), tolerance = 1e-6)
  m2 <- glm(cnt ~ poly(x, 3, raw = TRUE) + z, data = d, family = poisson())
  expect_no_warning(out2 <- check_collinearity(m2))
  expect_equal(out2$VIF, unname(.gvif_by_map(stats::vcov(m2), map)), tolerance = 1e-6)

  # raw polynomials whose correlation matrix is numerically singular, while
  # the correlation between terms is low
  d3 <- data.frame(z = seq(1, 100, length.out = 300), x2 = rnorm(300))
  d3$y <- 0.01 * d3$z + d3$x2 + rnorm(300)
  m4 <- lm(y ~ poly(z, 12, raw = TRUE) + x2, data = d3)
  v4 <- stats::vcov(m4)[-1, -1]
  expect_lt(rcond(stats::cov2cor(v4)), .Machine$double.eps)
  map4 <- c(stats::setNames(rep("poly", 12), colnames(v4)[1:12]), x2 = "x2")
  expect_no_warning(out4 <- check_collinearity(m4))
  expect_equal(out4$VIF, unname(.gvif_by_map(v4, map4)), tolerance = 1e-4)
  # columns of one term that cannot be made uncorrelated
  d4 <- data.frame(z = 1:100, x2 = rnorm(100))
  d4$y <- 0.01 * d4$z + d4$x2 + rnorm(100)
  m5 <- lm(y ~ poly(z, 12, raw = TRUE) + x2, data = d4)
  expect_no_warning(out5 <- check_collinearity(m5))
  expect_identical(out5$Term, c("poly(z, 12, raw = TRUE)", "x2"))

  # very high, but not singular, collinearity between separate terms
  d2 <- data.frame(x = runif(300, 0, 10), z = rnorm(300))
  d2$y <- d2$x + rnorm(300)
  m3 <- lm(
    y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + z,
    data = d2
  )
  expect_no_warning(out3 <- check_collinearity(m3))
  expect_true(all(out3$VIF >= 1))
})

test_that("check_collinearity, brglm2 models with transformed terms", {
  skip_if_not_installed("brglm2")
  d <- .multinom_data()
  d$o <- ordered(d$y)
  slopes <- c(
    x1 = "x1",
    `log(abs(x3))` = "log(abs(x3))",
    `poly(x2, 2)1` = "poly(x2, 2)",
    `poly(x2, 2)2` = "poly(x2, 2)",
    fq = "f",
    fr = "f"
  )
  terms <- c("x1", "log(abs(x3))", "poly(x2, 2)", "f")
  prefixed <- function(prefixes) {
    out <- rep(slopes, each = length(prefixes))
    names(out) <- paste(prefixes, rep(names(slopes), each = length(prefixes)), sep = ":")
    out
  }
  f <- o ~ x1 + log(abs(x3)) + poly(x2, 2) + f

  m1 <- brglm2::brmultinom(stats::update(f, y ~ .), data = d)
  out1 <- check_collinearity(m1)
  expect_identical(out1$Term, terms)
  expect_equal(
    out1$VIF,
    unname(.gvif_by_map(stats::vcov(m1), prefixed(c("b", "c")))),
    tolerance = 1e-8
  )

  m2 <- brglm2::bracl(f, data = d, parallel = FALSE)
  out2 <- check_collinearity(m2)
  expect_identical(out2$Term, terms)
  expect_equal(
    out2$VIF,
    unname(.gvif_by_map(stats::vcov(m2), prefixed(c("a", "b")))),
    tolerance = 1e-8
  )

  m3 <- brglm2::bracl(f, data = d, parallel = TRUE)
  out3 <- check_collinearity(m3)
  expect_identical(out3$Term, terms)
  expect_equal(out3$VIF, unname(.gvif_by_map(stats::vcov(m3), slopes)), tolerance = 1e-8)
})

test_that("check_collinearity, brglm2::bracl with a level named like a predictor", {
  skip_if_not_installed("brglm2")
  d <- .multinom_data(levels = c("x1", "b", "c"))
  d$o <- ordered(d$y)
  # with parallel = FALSE, all slopes have the level prefix
  m1 <- brglm2::bracl(o ~ x1 * f, data = d, parallel = FALSE)
  out1 <- check_collinearity(m1)
  expect_identical(out1$Term, c("x1", "f", "x1:f"))
  map1 <- c(fq = "f", fr = "f", `x1:fq` = "x1:f", `x1:fr` = "x1:f")
  map1 <- c(
    `x1:x1` = "x1",
    `b:x1` = "x1",
    stats::setNames(
      rep(map1, each = 2),
      paste(c("x1", "b"), rep(names(map1), each = 2), sep = ":")
    )
  )
  expect_equal(out1$VIF, unname(.gvif_by_map(stats::vcov(m1), map1)), tolerance = 1e-8)

  # with parallel = TRUE, level "x1" with "fq" and the plain slope "x1:fq"
  # give the same name, so the terms cannot be matched
  m2 <- brglm2::bracl(o ~ x1 * f, data = d, parallel = TRUE)
  expect_message(
    out2 <- check_collinearity(m2),
    "Could not extract model terms"
  )
  expect_null(out2)
})

test_that("check_collinearity, VGAM::vgam is not treated as vglm", {
  skip_if_not_installed("VGAM")
  d <- .multinom_data()
  m <- VGAM::vgam(y ~ VGAM::s(x1) + x3, family = VGAM::multinomial, data = d)
  expect_false(.is_vglm_categorical(m))
})
