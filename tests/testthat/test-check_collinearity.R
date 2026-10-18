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
      " gear 1.53 [1.04,  2.06]     1.24      0.65     [0.48, 0.96]",
      "",
      "Moderate Correlation",
      "",
      " Term  VIF    VIF 95% CI adj. VIF Tolerance Tolerance 95% CI",
      "   wt 5.05 [2.57,  7.07]     2.25      0.20     [0.14, 0.39]",
      "  cyl 5.41 [2.74,  7.58]     2.33      0.18     [0.13, 0.36]",
      " disp 9.97 [4.90, 13.93]     3.16      0.10     [0.07, 0.20]"
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
    c(1.04727, 1.15082, 1.10641),
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
    c(1.04727, 1.15082, 1.10641, 1.1879, 1, 1.17896),
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
    c(0.84182, 1, 0.84821),
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
  expect_message(ccoW <- check_collinearity(aW)) # nolint
  expect_message(ccoB <- check_collinearity(aB), regexp = NA) # nolint

  expect_identical(nrow(ccoM), 15L)
  expect_identical(nrow(ccoW), 3L)
  expect_identical(nrow(ccoB), 3L)
  # within-subject terms are orthogonal, so the VIF is 1 up to rounding and
  # the interval is [1, 1]
  expect_equal(ccoW$VIF_CI_low, c(1, 1, 1))
  expect_equal(ccoW$VIF_CI_high, c(1, 1, 1))

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
  expect_message(ccoW <- check_collinearity(aW)) # nolint
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
  # without a zero-inflation formula, there is no zero-inflation part to report
  expect_no_message(out <- check_collinearity(mod_trunc_error))
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

test_that("check_collinearity, adjusted vif with factors outside the model terms", {
  skip_if_not_installed("car")
  skip_if_not_installed("lme4")
  skip_if_not_installed("MASS")
  d_mt <- mtcars
  d_mt$am <- factor(d_mt$am)
  d_mt$cyl <- factor(d_mt$cyl)
  d_mt$gear <- factor(d_mt$gear)
  d_mt$g <- factor(rep(1:4, 8))

  # factor response
  mod <- glm(am ~ cyl + wt + hp, data = d_mt, family = binomial)
  out1 <- car::vif(mod)
  out2 <- check_collinearity(mod)
  expect_equal(out1[, 1], out2$VIF, tolerance = 1e-6, ignore_attr = TRUE)
  expect_equal(out1[, 3], out2$SE_factor, tolerance = 1e-6, ignore_attr = TRUE)

  # ordered response
  mod <- MASS::polr(gear ~ cyl + wt + hp, data = d_mt, Hess = TRUE)
  out <- check_collinearity(mod)
  expect_identical(out$Term, c("cyl", "wt", "hp"))
  expect_equal(out$SE_factor, out$VIF^(1 / (2 * c(2, 1, 1))), tolerance = 1e-8)

  # grouping factor of a mixed model
  mod <- suppressMessages(lme4::lmer(mpg ~ cyl + wt + hp + (1 | g), data = d_mt))
  out <- check_collinearity(mod)
  expect_identical(out$Term, c("cyl", "wt", "hp"))
  expect_equal(out$SE_factor, out$VIF^(1 / (2 * c(2, 1, 1))), tolerance = 1e-8)
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
  skip_on_cran()
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
  expect_equal(out1$VIF, check_collinearity(m_base)$VIF, tolerance = 1e-1)

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

  # raw polynomials with a badly conditioned correlation matrix, while the
  # correlation between terms is low. The degree keeps the condition number
  # (about 1e8) far from 1 / .Machine$double.eps, so the result does not
  # depend on the BLAS
  d3 <- data.frame(z = seq(1, 100, length.out = 300), x2 = rnorm(300))
  d3$y <- 0.01 * d3$z + d3$x2 + rnorm(300)
  m4 <- lm(y ~ poly(z, 6, raw = TRUE) + x2, data = d3)
  v4 <- stats::vcov(m4)[-1, -1]
  expect_lt(rcond(stats::cov2cor(v4)), 1e-8)
  map4 <- c(stats::setNames(rep("poly", 6), colnames(v4)[1:6]), x2 = "x2")
  expect_no_warning(out4 <- check_collinearity(m4))
  expect_equal(out4$VIF, unname(.gvif_by_map(v4, map4)), tolerance = 1e-6)

  # the check for singularity between terms, on exact matrices
  # columns 1 and 2 are one term whose correlation matrix is numerically
  # singular (rcond below .Machine$double.eps), and its whitening is exact
  a <- 1 - 2^-53
  r0 <- diag(3)
  r0[1, 2] <- r0[2, 1] <- a
  expect_lt(rcond(r0), .Machine$double.eps)
  expect_false(.is_singular_between_terms(r0, c(1, 1, 2)))
  # the same columns as separate terms are singular
  expect_true(.is_singular_between_terms(r0, 1:3))
  r <- matrix(c(1, 0.5, 0.5, 0.5, 1, 1, 0.5, 1, 1), 3, 3)
  # columns 2 and 3 are one term that cannot be made uncorrelated
  expect_false(.is_singular_between_terms(r, c(1, 2, 2)))
  # without a term for each column
  expect_false(.is_singular_between_terms(r, c(1, 2)))
  # the same columns as two terms are singular
  expect_true(.is_singular_between_terms(r, c(1, 2, 3)))
  # a third term equal to the first one
  r2 <- matrix(c(1, 0.5, 1, 0.5, 1, 0.5, 1, 0.5, 1), 3, 3)
  expect_true(.is_singular_between_terms(r2, 1:3))
  expect_false(.is_singular_between_terms(r2[1:2, 1:2], 1:2))

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

# Confidence intervals for VIF -------------------------------------------------

# lm fits for the interval tests: k = 1 at n = 32, k = 3 at n = 32, k = 3 at
# n = 300 with VIFs near 1 and above 10, and k = 1 at n = 100 with VIFs near 1
.vif_ci_fits <- function() {
  set.seed(1)
  n <- 300
  d <- data.frame(a = rnorm(n), b = rnorm(n), c = rnorm(n))
  d$d <- d$a + 0.3 * rnorm(n)
  d$y <- rnorm(n)
  set.seed(123)
  n <- 100
  d4 <- data.frame(x1 = rnorm(n), x2 = rnorm(n))
  d4$y <- 0.5 * d4$x1 + 0.5 * d4$x2 + rnorm(n)
  list(
    m1 = lm(mpg ~ wt + hp, data = mtcars),
    m2 = lm(mpg ~ wt + cyl + gear + disp, data = mtcars),
    m3 = lm(y ~ a + b + c + d, data = d),
    m4 = lm(y ~ x1 + x2, data = d4)
  )
}

test_that("check_collinearity, VIF CI matches MBESS::ci.R2 for fixed predictors", {
  # Reference values from MBESS 5.0.1, computed for each fit `m` in
  # `.vif_ci_fits()` with
  #   vif <- check_collinearity(m)$VIF
  #   sapply(vif, function(v) {
  #     r <- MBESS::ci.R2(
  #       R2 = 1 - 1 / v, N = insight::n_obs(m), K = length(vif) - 1,
  #       conf.level = 0.95, Random.Predictors = FALSE
  #     )
  #     1 / (1 - c(r$Lower.Conf.Limit.R2, r$Upper.Conf.Limit.R2))
  #   })
  # The interval inverts the noncentral F distribution of the auxiliary
  # regression's F statistic (Kelley 2007, Equations 68, 73, 75 and 76).
  fits <- .vif_ci_fits()
  mbess <- list(
    m1 = list(
      low = c(1.189741123, 1.189741123),
      high = c(2.561565375, 2.561565375)
    ),
    m2 = list(
      low = c(2.568411510, 2.740063225, 1.038860764, 4.896194597),
      high = c(7.065834021, 7.578944965, 2.062516798, 13.932600910)
    ),
    m3 = list(
      low = c(9.031201915, 1, 1, 9.024001139),
      high = c(12.449670149, 1.017152284, 1.027871649, 12.439712065)
    ),
    m4 = list(
      low = c(1, 1),
      high = c(1.058597355, 1.058597355)
    )
  )
  for (nm in names(fits)) {
    out <- check_collinearity(fits[[nm]], verbose = FALSE)
    expect_equal(out$VIF_CI_low, mbess[[nm]]$low, tolerance = 1e-5)
    expect_equal(out$VIF_CI_high, mbess[[nm]]$high, tolerance = 1e-5)
  }
})

test_that("check_collinearity, VIF CI limits solve the noncentral F equations", {
  # for a limit VIF_b, the noncentrality is n * (VIF_b - 1), and the limits
  # solve P(F <= F_obs | lambda) = 0.975 (lower) and 0.025 (upper)
  fits <- .vif_ci_fits()
  for (m in fits) {
    out <- check_collinearity(m, verbose = FALSE)
    n <- insight::n_obs(m)
    k <- nrow(out) - 1
    r2 <- 1 - 1 / out$VIF
    f_obs <- (r2 / k) / ((1 - r2) / (n - k - 1))
    for (i in seq_len(nrow(out))) {
      lambda_low <- n * (out$VIF_CI_low[i] - 1)
      lambda_high <- n * (out$VIF_CI_high[i] - 1)
      if (lambda_low > 0) {
        expect_equal(
          stats::pf(f_obs[i], k, n - k - 1, ncp = lambda_low),
          0.975,
          tolerance = 1e-6
        )
      }
      expect_gt(lambda_high, 0)
      expect_equal(
        stats::pf(f_obs[i], k, n - k - 1, ncp = lambda_high),
        0.025,
        tolerance = 1e-6
      )
    }
  }
})

test_that("check_collinearity, VIF CI is finite near VIF = 1", {
  # https://github.com/easystats/performance/issues/597
  set.seed(123)
  n <- 100
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  y <- 0.5 * x1 + 0.5 * x2 + rnorm(n)
  out <- check_collinearity(lm(y ~ x1 + x2), verbose = FALSE)
  expect_true(all(is.finite(out$VIF_CI_high)))
  expect_true(all(out$VIF_CI_high < 1.1))

  set.seed(1)
  d <- data.frame(y = rnorm(300), a = rnorm(300), b = rnorm(300))
  out <- check_collinearity(lm(y ~ a + b, data = d), verbose = FALSE)
  expect_true(all(is.finite(out$VIF_CI_high)))
  expect_true(all(out$VIF_CI_high < 1.1))
})

test_that("check_collinearity, VIF CI is [1, 1] for VIF = 1 and very small F", {
  # orthogonal predictors in a balanced design give VIF = 1 exactly
  d <- expand.grid(a = c(-1, 1), b = c(-1, 1), rep = 1:5)
  set.seed(2)
  d$y <- rnorm(nrow(d))
  out <- check_collinearity(lm(y ~ a + b, data = d), verbose = FALSE)
  expect_equal(out$VIF, c(1, 1))
  expect_identical(out$VIF_CI_low, c(1, 1))
  expect_identical(out$VIF_CI_high, c(1, 1))

  # the observed F is below the 0.025 quantile of the central F distribution,
  # so both limits for the population R2 are 0
  set.seed(1)
  n <- 100
  d <- data.frame(x1 = rnorm(n), x2 = rnorm(n))
  d$y <- rnorm(n)
  out <- check_collinearity(lm(y ~ x1 + x2, data = d), verbose = FALSE)
  r2 <- 1 - 1 / out$VIF
  expect_true(all(r2 / ((1 - r2) / (n - 2)) < stats::qf(0.025, 1, n - 2)))
  expect_identical(out$VIF_CI_low, c(1, 1))
  expect_identical(out$VIF_CI_high, c(1, 1))
  expect_identical(out$Tolerance_CI_low, c(1, 1))
  expect_identical(out$Tolerance_CI_high, c(1, 1))
})

test_that("check_collinearity, VIF CI for a term with more than one coefficient", {
  m <- lm(mpg ~ wt + factor(cyl), data = mtcars)
  out <- check_collinearity(m, verbose = FALSE)
  expect_true(all(is.finite(out$VIF_CI_low)))
  expect_true(all(is.finite(out$VIF_CI_high)))
  expect_true(all(out$VIF_CI_low <= out$VIF_CI_high))
})

test_that("check_collinearity, VIF CI covers the population VIF at the nominal rate", {
  skip_on_cran()
  # The other predictors are fixed. In each replicate, the first predictor is
  # drawn as x_other %*% b + N(0, 1), so the noncentrality of its auxiliary
  # regression is lambda = b' X'X b with centered X, and the population VIF is
  # 1 + lambda / n (Kelley 2007, Equations 73 and 75).
  .coverage <- function(n, vif, k, n_rep = 1000) {
    set.seed(123)
    x_other <- matrix(rnorm(n * k), n, k, dimnames = list(NULL, paste0("z", seq_len(k))))
    x_centered <- scale(x_other, scale = FALSE)
    s <- sqrt((vif - 1) * n / sum((x_centered %*% rep(1, k))^2))
    b <- rep(s, k)
    covered <- vapply(
      seq_len(n_rep),
      function(i) {
        d <- data.frame(x_other)
        d$x1 <- drop(x_other %*% b) + rnorm(n)
        d$y <- rnorm(n)
        m <- lm(stats::reformulate(c("x1", colnames(x_other)), "y"), data = d)
        out <- check_collinearity(m, verbose = FALSE)
        out$VIF_CI_low[1] <= vif && vif <= out$VIF_CI_high[1]
      },
      logical(1)
    )
    mean(covered)
  }
  cells <- data.frame(
    n = c(30, 30, 300, 300, 30),
    vif = c(1.02, 2, 1.02, 2, 2),
    k = c(1, 1, 1, 1, 3)
  )
  for (i in seq_len(nrow(cells))) {
    rate <- .coverage(cells$n[i], cells$vif[i], cells$k[i])
    expect_gte(rate, 0.93)
    expect_lte(rate, 0.97)
  }
})

test_that(".pf_ncp approximation for a large noncentrality", {
  # above a noncentrality of 1e5, .pf_ncp() uses the cube-root normal
  # approximation. Up to about 1e6 stats::pf() is still accurate, so both
  # must agree there
  for (ncp in c(2e5, 5e5)) {
    for (df1 in c(1, 5)) {
      for (df2 in c(28, 296)) {
        q <- (ncp + df1) / df1 * c(0.99, 1, 1.01)
        expect_equal(
          vapply(q, .pf_ncp, numeric(1), df1 = df1, df2 = df2, ncp = ncp),
          stats::pf(q, df1, df2, ncp = ncp),
          tolerance = 1e-6
        )
      }
    }
  }

  # VIF 1000 at n = 300 and k = 3 needs a noncentrality of about 2.5e5 and
  # 3.5e5 for its limits. Reference values from MBESS 5.0.1:
  #   r <- MBESS::ci.R2(
  #     R2 = 1 - 1 / 1000, N = 300, K = 3,
  #     conf.level = 0.95, Random.Predictors = FALSE
  #   )
  #   1 / (1 - c(r$Lower.Conf.Limit.R2, r$Upper.Conf.Limit.R2))
  set.seed(1)
  d <- data.frame(
    y = rnorm(300),
    a = rnorm(300),
    b = rnorm(300),
    c = rnorm(300),
    e = rnorm(300)
  )
  m <- lm(y ~ a + b + c + e, data = d)
  out <- .vif_ci(m, rep(1000, 4), ci = 0.95, n_coef = rep(1, 4))
  expect_gt(300 * (1 / (1 - out$CI_low[1]) - 1), 1e5)
  expect_equal(
    1 / (1 - c(out$CI_low[1], out$CI_high[1])),
    c(834.1221966, 1151.8537036),
    tolerance = 1e-5
  )
})

test_that(".vif_ci handles infinite VIFs and failed integration", {
  # an infinite VIF gives R2 limits of 1 (VIF limits Inf), not NaN
  m <- lm(mpg ~ wt + hp, data = mtcars)
  out <- .vif_ci(m, c(Inf, 2), ci = 0.95, n_coef = c(1, 1))
  expect_identical(out$CI_low[1], 1)
  expect_identical(out$CI_high[1], 1)
  expect_false(anyNA(out$CI_low[2]))

  # with n - k - 1 = 1 and a huge F, integrate() fails in the root search,
  # and the limit is NA instead of an error
  d <- data.frame(y = c(1, 3, 2), a = c(1, 2, 3), b = c(2, 1, 4))
  m <- lm(y ~ a + b, data = d)
  expect_error(.ncp_limit(1e12, 1, 1, 0.975), "divergent")
  out <- .vif_ci(m, c(1e12, 1e12), ci = 0.95, n_coef = c(1, 1))
  expect_true(all(is.na(out$CI_low)))
})

test_that("check_collinearity, VIF CI is NA without a CI level", {
  m <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
  ci_cols <- c("VIF_CI_low", "VIF_CI_high", "Tolerance_CI_low", "Tolerance_CI_high")
  for (ci in list(NULL, NA)) {
    out <- check_collinearity(m, ci = ci, verbose = FALSE)
    expect_true(all(is.na(unlist(out[ci_cols]))))
  }
})

# ordinal models whose variance-covariance matrix holds thresholds, not one
# intercept. x3 is x1 plus noise, x2 is independent of both.
.ordinal_vif_data <- function() {
  set.seed(1234)
  n <- 500
  d <- data.frame(
    x1 = stats::rnorm(n),
    x2 = stats::rnorm(n),
    f = factor(sample(c("p", "q", "r"), n, replace = TRUE)),
    g = factor(rep(1:25, length.out = n))
  )
  d$x3 <- d$x1 + stats::rnorm(n, sd = 0.5)
  d$x4 <- 2 * d$x1
  # a group effect, so that clmm and clmm2 can estimate the random intercept
  group_effect <- stats::rnorm(25, sd = 0.5)[d$g]
  d$o <- cut(
    d$x1 + d$x2 + group_effect + stats::rnorm(n),
    breaks = 4,
    ordered_result = TRUE,
    labels = c("a", "b", "c", "d")
  )
  d
}

# VIF of single-coefficient terms from the slope block of the
# variance-covariance matrix, as in car::vif.default()
.slope_block_vif <- function(v, slopes) {
  diag(solve(stats::cov2cor(v[slopes, slopes])))
}

.expect_vif_structure <- function(out) {
  vif <- stats::setNames(out$VIF, out$Term)
  expect_identical(out$Term, c("x1", "x2", "x3"))
  expect_lt(vif[["x2"]], vif[["x1"]] / 2)
  expect_lt(vif[["x2"]], vif[["x3"]] / 2)
  expect_true(all(is.finite(out$VIF_CI_low)))
  expect_true(all(is.finite(out$VIF_CI_high)))
  expect_true(all(out$VIF_CI_low <= out$VIF & out$VIF <= out$VIF_CI_high))
}

test_that("check_collinearity, polr and svyolr match car::vif", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("survey")
  skip_if_not_installed("car")
  d <- .ordinal_vif_data()
  des <- survey::svydesign(ids = ~1, data = d, weights = rep(1, nrow(d)))
  models <- list(
    logistic = MASS::polr(o ~ x1 + f + x2 + x3, data = d, Hess = TRUE),
    probit = MASS::polr(o ~ x1 + f + x2 + x3, data = d, Hess = TRUE, method = "probit"),
    svyolr = survey::svyolr(o ~ x1 + f + x2 + x3, design = des)
  )
  for (m in models) {
    out <- check_collinearity(m)
    expect_identical(out$Term, c("x1", "f", "x2", "x3"))
    expect_equal(out$VIF, car::vif(m)[, "GVIF"], tolerance = 1e-6, ignore_attr = TRUE)
  }
})

test_that("check_collinearity, polr, clm2 and clmm2 match clm and clmm", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("ordinal")
  skip_if_not_installed("withr")
  withr::local_package("ordinal")
  d <- .ordinal_vif_data()
  for (fo in list(o ~ x1 + f + x2 + x3, o ~ x3 + f + x1 + x2)) {
    # put the formula itself into each call, because clm2() evaluates its
    # formula argument again and cannot find a local variable
    ref <- check_collinearity(eval(bquote(ordinal::clm(.(fo), data = d))))
    for (m in list(
      eval(bquote(MASS::polr(.(fo), data = d, Hess = TRUE))),
      eval(bquote(ordinal::clm2(.(fo), data = d, Hess = TRUE)))
    )) {
      out <- check_collinearity(m)
      expect_identical(out$Term, ref$Term)
      expect_equal(out$VIF, ref$VIF, tolerance = 1e-4)
    }
  }
  ref <- check_collinearity(ordinal::clmm(o ~ x1 + f + x2 + x3 + (1 | g), data = d))
  out <- check_collinearity(
    ordinal::clmm2(o ~ x1 + f + x2 + x3, random = g, data = d, Hess = TRUE)
  )
  expect_identical(out$Term, c("x1", "f", "x2", "x3"))
  expect_identical(out$Term, ref$Term)
  expect_equal(out$VIF, ref$VIF, tolerance = 1e-4)
})

test_that("check_collinearity, ordinal models with thresholds keep VIFs on their terms", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("survey")
  skip_if_not_installed("ordinal")
  skip_if_not_installed("withr")
  withr::local_package("ordinal")
  d <- .ordinal_vif_data()
  des <- survey::svydesign(ids = ~1, data = d, weights = rep(1, nrow(d)))
  models <- list(
    MASS::polr(o ~ x1 + x2 + x3, data = d, Hess = TRUE),
    survey::svyolr(o ~ x1 + x2 + x3, design = des),
    ordinal::clm2(o ~ x1 + x2 + x3, data = d, Hess = TRUE),
    ordinal::clmm2(o ~ x1 + x2 + x3, random = g, data = d, Hess = TRUE)
  )
  for (m in models) {
    .expect_vif_structure(check_collinearity(m))
  }
})

test_that("check_collinearity, stan_polr", {
  skip_on_cran()
  skip_if_not_installed("rstanarm")
  d <- .ordinal_vif_data()
  m <- suppressWarnings(rstanarm::stan_polr(
    o ~ x1 + x2 + x3,
    data = d,
    prior = rstanarm::R2(0.3),
    chains = 1,
    iter = 1000,
    refresh = 0,
    seed = 1234
  ))
  out <- check_collinearity(m)
  slopes <- c("x1", "x2", "x3")
  expect_equal(
    out$VIF,
    .slope_block_vif(insight::get_varcov(m), slopes),
    tolerance = 1e-6,
    ignore_attr = TRUE
  )
  .expect_vif_structure(out)
})

test_that("check_collinearity, polr and clm with an aliased predictor", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("ordinal")
  skip_if_not_installed("car")
  d <- .ordinal_vif_data()
  ref <- car::vif(MASS::polr(o ~ x1 + x2 + x3, data = d, Hess = TRUE))
  models <- list(
    polr = suppressWarnings(MASS::polr(o ~ x1 + x4 + x2 + x3, data = d, Hess = TRUE)),
    clm = ordinal::clm(o ~ x1 + x4 + x2 + x3, data = d)
  )
  tolerances <- c(polr = 1e-6, clm = 1e-4)
  for (i in names(models)) {
    warns <- capture_warnings(out <- check_collinearity(models[[i]]))
    expect_length(warns, 1)
    expect_match(warns, "rank deficient", fixed = TRUE)
    expect_match(warns, "x4", fixed = TRUE)
    expect_identical(out$Term, c("x1", "x2", "x3"))
    expect_equal(out$VIF, ref, tolerance = tolerances[[i]], ignore_attr = TRUE)
  }
})

test_that("check_collinearity, ordinal models with backticks, unused levels, or other contrasts", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("ordinal")
  skip_if_not_installed("car")
  skip_if_not_installed("withr")
  withr::local_package("ordinal")
  d <- .ordinal_vif_data()

  # term names with backticks
  d$`x 1` <- d$x1
  m <- MASS::polr(o ~ `x 1` + x2 + x3, data = d, Hess = TRUE)
  out <- check_collinearity(m)
  expect_identical(out$Term, c("x 1", "x2", "x3"))
  expect_equal(out$VIF, car::vif(m), tolerance = 1e-6, ignore_attr = TRUE)

  # a factor level that only rows outside the fit have is not aliased
  d$f4 <- factor(ifelse(d$x2 > 1.5, "s", as.character(d$f)))
  d_fit <- d[d$x2 <= 1.5, ]
  d_fit$f4 <- droplevels(d_fit$f4)
  ref <- check_collinearity(ordinal::clm(o ~ x1 + f4 + x2 + x3, data = d_fit))
  expect_silent(
    out <- check_collinearity(ordinal::clm(
      o ~ x1 + f4 + x2 + x3,
      data = d,
      subset = x2 <= 1.5
    ))
  )
  expect_identical(out$Term, ref$Term)
  expect_equal(out$VIF, ref$VIF, tolerance = 1e-4)

  # the generalized VIF of a factor does not depend on its contrasts
  ref <- check_collinearity(ordinal::clm(o ~ x1 + f + x2 + x3, data = d))
  sum_contrasts <- list(f = "contr.sum")
  models <- list(
    ordinal::clm(o ~ x1 + f + x2 + x3, data = d, contrasts = sum_contrasts),
    ordinal::clm2(o ~ x1 + f + x2 + x3, data = d, contrasts = sum_contrasts, Hess = TRUE),
    withr::with_options(
      list(contrasts = c("contr.sum", "contr.poly")),
      MASS::polr(o ~ x1 + f + x2 + x3, data = d, Hess = TRUE)
    )
  )
  for (m in models) {
    out <- check_collinearity(m)
    expect_identical(out$Term, ref$Term)
    expect_equal(out$VIF, ref$VIF, tolerance = 1e-4)
  }
})

# generalized VIFs from the correlation matrix of the coefficients, with the
# term blocks taken from the "assign" attribute of a model matrix
.gvif_direct <- function(v, assign) {
  R <- stats::cov2cor(v)
  vapply(
    unique(assign),
    function(term) {
      s <- which(assign == term)
      det(R[s, s, drop = FALSE]) * det(R[-s, -s, drop = FALSE]) / det(R)
    },
    numeric(1)
  )
}

.zi_test_data <- function(n = 600) {
  set.seed(123)
  d <- data.frame(x1 = rnorm(n), g = factor(sample(1:15, n, replace = TRUE)))
  d$x2 <- 0.6 * d$x1 + rnorm(n)
  d$f <- factor(
    ifelse(d$x1 + rnorm(n) > 0.5, "c", ifelse(d$x2 + rnorm(n) > 0, "b", "a"))
  )
  p_zero <- stats::plogis(-0.5 + 0.8 * d$x2 - 0.5 * d$x1 + 0.4 * (d$f == "b"))
  d$y <- stats::rpois(n, exp(0.5 + 0.3 * d$x1)) * stats::rbinom(n, 1, 1 - p_zero)
  d
}

test_that("check_collinearity, glmmTMB zero-inflation terms differ from the conditional terms", {
  skip_if_not_installed("glmmTMB")
  d <- .zi_test_data()
  mm <- stats::model.matrix(~ x2 + x1 + f, data = d)
  assign <- attr(mm, "assign")[-1]

  for (cond in list(y ~ x1, y ~ x1 + f, y ~ f + x2)) {
    m <- glmmTMB::glmmTMB(cond, ziformula = ~ x2 + x1 + f, data = d, family = poisson())
    v <- vcov(m)$zi[-1, -1]
    out <- check_collinearity(m, component = "zero_inflated")
    expect_identical(out$Term, c("x2", "x1", "f"))
    expect_equal(out$VIF, .gvif_direct(v, assign), tolerance = 1e-8)
  }
})

test_that("check_collinearity, glmmTMB zero-inflation terms with transformations, interactions, random effects, or missing values", {
  skip_if_not_installed("glmmTMB")
  d <- .zi_test_data()
  d_na <- d
  d_na$x2[c(3, 50, 200, 333, 512)] <- NA

  cases <- list(
    list(zi = ~ log(x2 + 5) + x1 + f, fixed = ~ log(x2 + 5) + x1 + f, data = d),
    list(zi = ~ x2 * f + x1, fixed = ~ x2 * f + x1, data = d),
    list(zi = ~ x2 + x1 + f + (1 | g), fixed = ~ x2 + x1 + f, data = d),
    list(zi = ~ x2 + x1 + f, fixed = ~ x2 + x1 + f, data = d_na)
  )
  for (case in cases) {
    m <- glmmTMB::glmmTMB(
      y ~ x1,
      ziformula = case$zi,
      data = case$data,
      family = poisson()
    )
    assign <- attr(stats::model.matrix(case$fixed, data = case$data), "assign")[-1]
    v <- vcov(m)$zi[-1, -1]
    out <- check_collinearity(m, component = "zero_inflated", verbose = FALSE)
    expect_equal(out$VIF, .gvif_direct(v, assign), tolerance = 1e-8)
    if (identical(case$fixed, ~ x2 + x1 + f)) {
      expect_identical(out$Term, c("x2", "x1", "f"))
    }
  }
})

test_that("check_collinearity, glmmTMB zero-inflation component with one term", {
  skip_if_not_installed("glmmTMB")
  d <- .zi_test_data()
  m <- glmmTMB::glmmTMB(y ~ x1 + f, ziformula = ~x2, data = d, family = poisson())

  out_cond <- check_collinearity(m, component = "conditional", verbose = FALSE)
  out_all <- check_collinearity(m, component = "all", verbose = FALSE)
  expect_identical(out_all$Component, rep("conditional", nrow(out_cond)))
  out_all$Component <- NULL
  expect_equal(out_all, out_cond)

  expect_message(
    check_collinearity(m, component = "all"),
    "Not enough model terms in the zero-inflat"
  )
  expect_no_message(check_collinearity(m, component = "all", verbose = FALSE))
})

test_that("check_collinearity, glmmTMB zero-inflation component without VIFs for other reasons", {
  skip_if_not_installed("glmmTMB")
  d <- .zi_test_data()
  m <- glmmTMB::glmmTMB(y ~ x1 + f, ziformula = ~ x2 + x1, data = d, family = poisson())
  check_part <- .check_collinearity
  local_mocked_bindings(
    .check_collinearity = function(x, component, ...) {
      if (component == "zero_inflated") NULL else check_part(x, component, ...)
    }
  )
  expect_message(
    out <- check_collinearity(m, component = "all"),
    "Could not compute VIFs for the zero-inflation part"
  )
  expect_identical(out$Term, c("x1", "f"))

  # a zero-inflation formula with only a random effect has no fixed terms
  m <- glmmTMB::glmmTMB(y ~ x1 + f, ziformula = ~ (1 | g), data = d, family = poisson())
  expect_message(
    check_collinearity(m, component = "all"),
    "Not enough model terms in the zero-inflat"
  )
})

test_that("check_collinearity, glmmTMB zero-inflation formula with only an intercept", {
  skip_if_not_installed("glmmTMB")
  d <- .zi_test_data()
  m <- glmmTMB::glmmTMB(y ~ x1 + f, ziformula = ~1, data = d, family = poisson())
  out_cond <- check_collinearity(m, component = "conditional", verbose = FALSE)
  expect_message(
    out_all <- check_collinearity(m, component = "all"),
    "Not enough model terms in the zero-inflat"
  )
  expect_identical(out_all$Term, out_cond$Term)
  expect_equal(out_all$VIF, out_cond$VIF)
  expect_null(check_collinearity(m, component = "zero_inflated", verbose = FALSE))
})

test_that("check_collinearity, pscl zero-inflation formula with one term", {
  skip_if_not_installed("pscl")
  d <- .zi_test_data()
  models <- list(
    pscl::zeroinfl(y ~ x1 + f | x2, data = d),
    pscl::hurdle(y ~ x1 + f | x2, data = d)
  )
  for (m in models) {
    expect_message(
      out <- check_collinearity(m),
      "Not enough model terms in the zero-inflat"
    )
    expect_identical(out$Term, c("x1", "f"))
  }
})

test_that("check_collinearity, pscl zero-inflation terms differ from the count terms", {
  skip_if_not_installed("pscl")
  d <- .zi_test_data()
  assign <- attr(stats::model.matrix(~ x2 + x1 + f, data = d), "assign")[-1]

  models <- list(
    pscl::zeroinfl(y ~ x1 | x2 + x1 + f, data = d),
    pscl::hurdle(y ~ x1 | x2 + x1 + f, data = d)
  )
  for (m in models) {
    v <- vcov(m)
    zero_cols <- grep("^zero_", colnames(v))[-1]
    out <- check_collinearity(m, component = "zero_inflated")
    expect_identical(out$Term, c("x2", "x1", "f"))
    expect_equal(
      out$VIF,
      .gvif_direct(v[zero_cols, zero_cols], assign),
      tolerance = 1e-8
    )
  }
})
