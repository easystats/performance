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

  m <- insight::download_model("brms_2")
  skip_if(is.null(m))

  out <- check_collinearity(m, verbose = FALSE)
  expect_identical(out$Term, c("treat", "c2", "treat:c2"))

  R <- stats::cov2cor(stats::vcov(m)[-1, -1])
  expect_equal(out$VIF, unname(diag(solve(R))), tolerance = 1e-6)
})


test_that("check_collinearity, brms models with more than one intercept", {
  skip_on_cran()
  skip_if_not_installed("brms")
  skip_if_not_installed("curl")
  skip_if_offline()
  skip_if_not_installed("httr2")

  m <- insight::download_model("brms_ordinal_2")
  skip_if(is.null(m))

  out <- check_collinearity(m, verbose = FALSE)
  expect_identical(out$Term, c("treat", "period", "carry"))

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


test_that("check_collinearity, rank-deficient flag without NA columns", {
  m <- lm(mpg ~ wt + hp + disp, data = mtcars)
  v <- stats::vcov(m)
  expected <- check_collinearity(m)$VIF
  testthat::local_mocked_bindings(
    get_varcov = function(...) structure(v, rank_deficient = TRUE),
    .package = "insight"
  )
  expect_warning(
    out <- check_collinearity(m, verbose = TRUE),
    "Model matrix is rank deficient"
  )
  expect_equal(out$VIF, expected, tolerance = 1e-8)
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
  d_mt <- mtcars
  d_mt$am <- factor(d_mt$am)
  d_mt$cyl <- factor(d_mt$cyl)
  mod <- glm(am ~ cyl + wt + hp, data = d_mt, family = binomial)
  out1 <- car::vif(mod)
  out2 <- check_collinearity(mod)
  expect_equal(out1[, 1], out2$VIF, tolerance = 1e-6, ignore_attr = TRUE)
  expect_equal(out1[, 3], out2$SE_factor, tolerance = 1e-6, ignore_attr = TRUE)
})

test_that("check_collinearity, adjusted vif uses the number of coefficients of each term", {
  skip_if_not_installed("car")
  set.seed(123)
  d <- data.frame(
    x1 = rnorm(200),
    x2 = rnorm(200),
    f = factor(sample(letters[1:3], 200, replace = TRUE))
  )
  d$y <- d$x1 + d$x2 + rnorm(200)
  mod <- lm(y ~ poly(x2, 2) + x1 * f, data = d)
  out1 <- suppressMessages(car::vif(mod, type = "terms"))
  out2 <- check_collinearity(mod, verbose = FALSE)
  expect_identical(out2$Term, rownames(out1))
  expect_equal(
    out2$SE_factor,
    out1[, "GVIF^(1/(2*Df))"],
    tolerance = 1e-8,
    ignore_attr = TRUE
  )
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
  expect_identical(out$Term, c("x_continuous", "x_binary"))
  expect_equal(out$VIF, c(1.11, 1.11), tolerance = 0.05)
})

# generalized VIF of each block of columns of `v`, from the correlation matrix
# of all columns in `blocks`
.gvif_blocks <- function(v, blocks) {
  cols <- unlist(blocks)
  cor_mat <- stats::cov2cor(v[cols, cols])
  pos <- split(seq_along(cols), rep(seq_along(blocks), lengths(blocks)))
  vapply(
    pos,
    function(idx) {
      det(cor_mat[idx, idx, drop = FALSE]) *
        det(cor_mat[-idx, -idx, drop = FALSE]) /
        det(cor_mat)
    },
    numeric(1),
    USE.NAMES = FALSE
  )
}

# `map` maps the name of each slope column of `v` to its model term
.gvif_by_map <- function(v, map) {
  .gvif_blocks(v, split(names(map), factor(map, unique(map))))
}

.prefix_map <- function(map, prefixes, sep = ":") {
  stats::setNames(
    rep(map, each = length(prefixes)),
    paste(prefixes, rep(names(map), each = length(prefixes)), sep = sep)
  )
}

.clm_data <- function(n = 400) {
  set.seed(42)
  d <- data.frame(x1 = rnorm(n), x3 = rnorm(n), x4 = rnorm(n))
  d$x2 <- d$x1 + rnorm(n, sd = 0.7)
  latent <- 0.5 * d$x1 + 0.4 * d$x2 - 0.3 * d$x3 + stats::rlogis(n)
  d$y <- cut(
    latent,
    stats::quantile(latent, c(0, 0.25, 0.5, 0.75, 1)),
    include.lowest = TRUE,
    labels = c("a", "b", "c", "d"),
    ordered_result = TRUE
  )
  d
}

test_that("check_collinearity, ordinal clm models with scale effects", {
  skip_if_not_installed("ordinal")
  d <- .clm_data()
  m <- ordinal::clm(y ~ x1 + x2 + x3, scale = ~x1, data = d)
  expect_message(
    out <- check_collinearity(m),
    "scale effects \\(x1\\)"
  )
  expect_identical(out$Term, c("x1", "x2", "x3"))
  slope_cols <- length(m$alpha) + seq_along(m$beta)
  expected <- .gvif_blocks(stats::vcov(m), as.list(slope_cols))
  expect_equal(out$VIF, expected, tolerance = 1e-8)

  m2 <- ordinal::clm(y ~ x1, scale = ~x2, data = d)
  expect_no_message(
    check_collinearity(m2),
    message = "location effects only"
  )
})

test_that("check_collinearity, ordinal clm models with nominal effects", {
  skip_if_not_installed("ordinal")
  d <- .clm_data()
  m <- ordinal::clm(y ~ x1 + x2 + x4, nominal = ~x3, data = d)
  expect_message(
    out <- check_collinearity(m),
    "nominal effects \\(x3\\)"
  )
  expect_identical(out$Term, c("x1", "x2", "x4"))
  slope_cols <- length(m$alpha) + seq_along(m$beta)
  expected <- .gvif_blocks(stats::vcov(m), as.list(slope_cols))
  expect_equal(out$VIF, expected, tolerance = 1e-8)

  # clm drops a location term that is also a nominal effect
  m3 <- ordinal::clm(y ~ x1 + x2 + x3, nominal = ~x3, data = d)
  expect_no_warning(
    out3 <- suppressMessages(check_collinearity(m3))
  )
  expect_identical(out3$Term, c("x1", "x2"))
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
  out <- check_collinearity(m)
  expect_identical(out$Term, c("x1", "x3", "x2", "f", "x1:x3", "x1:f"))
  expected <- .gvif_by_map(stats::vcov(m), .prefix_map(.multinom_terms, c("b", "c")))
  expect_equal(out$VIF, expected, tolerance = 1e-6)
  m_coef <- c(2, 2, 2, 4, 2, 4)
  expect_equal(out$SE_factor, out$VIF^(1 / (2 * m_coef)), tolerance = 1e-6)
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
  expected <- .gvif_by_map(
    stats::vcov(m),
    .prefix_map(.multinom_terms, c("b", "c"), sep = "~")
  )
  expect_equal(out$VIF, expected, tolerance = 1e-6)
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
  d <- .multinom_data(levels = c("a", "b c", "d:e"))
  d$intercept_shift <- d$x3
  m <- nnet::multinom(y ~ x1 + x2 + intercept_shift, data = d, trace = FALSE)
  out <- check_collinearity(m)
  expect_identical(out$Term, c("x1", "x2", "intercept_shift"))
  map <- c(x1 = "x1", x2 = "x2", intercept_shift = "intercept_shift")
  expected <- .gvif_by_map(stats::vcov(m), .prefix_map(map, c("b c", "d:e")))
  expect_equal(out$VIF, expected, tolerance = 1e-6)
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

test_that("check_collinearity, brglm2::bracl", {
  skip_if_not_installed("brglm2")
  skip_on_cran()
  d <- .multinom_data()
  d$o <- ordered(d$y)
  m1 <- brglm2::bracl(o ~ x1 + x3 + x2 + f, data = d, parallel = FALSE)
  out1 <- check_collinearity(m1)
  expect_identical(out1$Term, c("x1", "x3", "x2", "f"))
  slopes <- c(x1 = "x1", x3 = "x3", x2 = "x2", fq = "f", fr = "f")
  expected1 <- .gvif_by_map(stats::vcov(m1), .prefix_map(slopes, c("a", "b")))
  expect_equal(out1$VIF, expected1, tolerance = 1e-8)
  m_base <- brglm2::brmultinom(y ~ x1 + x3 + x2 + f, data = d)
  expect_equal(out1$VIF, check_collinearity(m_base)$VIF, tolerance = 1e-1)
})

test_that("check_collinearity, VGAM::vglm categorical families", {
  skip_if_not_installed("VGAM")
  d <- .multinom_data()
  d$o <- cut(
    0.5 * d$x1 - 0.3 * d$x3 + 0.2 * (d$f == "q") + stats::rlogis(nrow(d)),
    c(-Inf, -0.5, 0.5, Inf),
    labels = c("a", "b", "c"),
    ordered_result = TRUE
  )
  d$count <- stats::rpois(nrow(d), exp(0.3 * d$x1 - 0.2 * d$x3))
  fits <- list(
    multinomial = VGAM::vglm(y ~ x1 + x2 + f, family = VGAM::multinomial, data = d),
    sratio = VGAM::vglm(o ~ x1 + x2 + f, family = VGAM::sratio(), data = d),
    poissonff = VGAM::vglm(count ~ x1 + x2 + f, family = VGAM::poissonff(), data = d)
  )
  for (i in names(fits)) {
    m <- fits[[i]]
    blocks <- attr(stats::model.matrix(m), "assign")
    blocks <- blocks[names(blocks) != "(Intercept)"]
    out <- check_collinearity(m)
    expect_identical(out$Term, c("x1", "x2", "f"), info = i)
    expected <- .gvif_blocks(stats::vcov(m), blocks)
    expect_equal(out$VIF, expected, tolerance = 1e-8, info = i)
    n_cols <- unname(lengths(blocks))
    expect_equal(out$SE_factor, out$VIF^(1 / (2 * n_cols)), tolerance = 1e-8, info = i)
  }
})

test_that("check_collinearity, multinomial models with backticked names", {
  skip_if_not_installed("nnet")
  d <- .multinom_data()
  d$`my x` <- d$x1
  d$my_x <- d$x1
  m <- nnet::multinom(y ~ `my x` + x2 + f + `my x`:f, data = d, trace = FALSE)
  m_renamed <- nnet::multinom(y ~ my_x + x2 + f + my_x:f, data = d, trace = FALSE)
  out <- suppressMessages(check_collinearity(m))
  out_renamed <- suppressMessages(check_collinearity(m_renamed))
  expect_identical(out$Term, c("my x", "x2", "f", "my x:f"))
  expect_equal(out$VIF, out_renamed$VIF, tolerance = 1e-8)
})

test_that("check_collinearity, multinomial models without intercept", {
  skip_if_not_installed("nnet")
  d <- .multinom_data()
  m <- nnet::multinom(y ~ 0 + x1 + x3 + x2, data = d, trace = FALSE)
  expect_message(
    out <- check_collinearity(m),
    "Model without intercept"
  )
  expect_identical(out$Term, c("x1", "x3", "x2"))
  map <- .prefix_map(c(x1 = "x1", x3 = "x3", x2 = "x2"), c("b", "c"))
  expect_equal(out$VIF, .gvif_by_map(stats::vcov(m), map), tolerance = 1e-8)
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
})

test_that("check_collinearity, strong correlation inside one term is not singular", {
  set.seed(123)
  d <- data.frame(z = seq(1, 100, length.out = 300), x2 = rnorm(300))
  d$y <- 0.01 * d$z + d$x2 + rnorm(300)
  m <- lm(y ~ poly(z, 6, raw = TRUE) + x2, data = d)
  v <- stats::vcov(m)[-1, -1]
  expect_lt(rcond(stats::cov2cor(v)), 1e-8)
  expect_no_warning(out <- check_collinearity(m))
  expect_equal(out$VIF, .gvif_blocks(v, list(1:6, 7)), tolerance = 1e-6)

  a <- 1 - 2^-53
  r0 <- diag(3)
  r0[1, 2] <- r0[2, 1] <- a
  expect_false(.is_singular_between_terms(r0, c(1, 1, 2)))
  expect_true(.is_singular_between_terms(r0, 1:3))
  r <- matrix(c(1, 0.5, 0.5, 0.5, 1, 1, 0.5, 1, 1), 3, 3)
  expect_false(.is_singular_between_terms(r, c(1, 2, 2)))
  expect_false(.is_singular_between_terms(r, c(1, 2)))
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

  m1 <- brglm2::brmultinom(y ~ x1 + log(abs(x3)) + poly(x2, 2) + f, data = d)
  out1 <- check_collinearity(m1)
  expect_identical(out1$Term, terms)
  expected1 <- .gvif_by_map(stats::vcov(m1), .prefix_map(slopes, c("b", "c")))
  expect_equal(out1$VIF, expected1, tolerance = 1e-8)

  m2 <- brglm2::bracl(o ~ x1 + log(abs(x3)) + poly(x2, 2) + f, data = d, parallel = TRUE)
  out2 <- check_collinearity(m2)
  expect_identical(out2$Term, terms)
  expect_equal(out2$VIF, .gvif_by_map(stats::vcov(m2), slopes), tolerance = 1e-8)
})

test_that("check_collinearity, brglm2::bracl with a level named like a predictor", {
  skip_if_not_installed("brglm2")
  d <- .multinom_data(levels = c("x1", "b", "c"))
  d$o <- ordered(d$y)
  m1 <- brglm2::bracl(o ~ x1 * f, data = d, parallel = FALSE)
  out1 <- check_collinearity(m1)
  expect_identical(out1$Term, c("x1", "f", "x1:f"))
  map1 <- c(
    `x1:x1` = "x1",
    `b:x1` = "x1",
    .prefix_map(c(fq = "f", fr = "f", `x1:fq` = "x1:f", `x1:fr` = "x1:f"), c("x1", "b"))
  )
  expect_equal(out1$VIF, .gvif_by_map(stats::vcov(m1), map1), tolerance = 1e-8)

  # with parallel = TRUE, level "x1" with "fq" and the plain slope "x1:fq"
  # give the same name
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

test_that("check_collinearity, VIF CI matches MBESS::ci.R2 for fixed predictors", {
  # reference values from MBESS::ci.R2(Random.Predictors = FALSE)
  set.seed(1)
  n <- 300
  d <- data.frame(a = rnorm(n), b = rnorm(n), c = rnorm(n))
  d$d <- d$a + 0.3 * rnorm(n)
  d$y <- rnorm(n)
  out <- check_collinearity(lm(y ~ a + b + c + d, data = d), verbose = FALSE)
  expect_equal(out$VIF_CI_low, c(9.031201915, 1, 1, 9.024001139), tolerance = 1e-5)
  expect_equal(
    out$VIF_CI_high,
    c(12.449670149, 1.017152284, 1.027871649, 12.439712065),
    tolerance = 1e-5
  )

  # https://github.com/easystats/performance/issues/597
  set.seed(123)
  n <- 100
  d <- data.frame(x1 = rnorm(n), x2 = rnorm(n))
  d$y <- 0.5 * d$x1 + 0.5 * d$x2 + rnorm(n)
  out <- check_collinearity(lm(y ~ x1 + x2, data = d), verbose = FALSE)
  expect_equal(out$VIF_CI_low, c(1, 1), tolerance = 1e-5)
  expect_equal(out$VIF_CI_high, c(1.058597355, 1.058597355), tolerance = 1e-5)
})

test_that("check_collinearity, VIF CI is [1, 1] for VIF = 1 and very small F", {
  d <- expand.grid(a = c(-1, 1), b = c(-1, 1), rep = 1:5)
  set.seed(2)
  d$y <- rnorm(nrow(d))
  out <- check_collinearity(lm(y ~ a + b, data = d), verbose = FALSE)
  expect_equal(out$VIF, c(1, 1))
  expect_identical(out$VIF_CI_low, c(1, 1))
  expect_identical(out$VIF_CI_high, c(1, 1))

  set.seed(1)
  n <- 100
  d <- data.frame(x1 = rnorm(n), x2 = rnorm(n))
  d$y <- rnorm(n)
  out <- check_collinearity(lm(y ~ x1 + x2, data = d), verbose = FALSE)
  expect_identical(out$VIF_CI_low, c(1, 1))
  expect_identical(out$VIF_CI_high, c(1, 1))
})

test_that("check_collinearity, VIF CI for a term with more than one coefficient", {
  m <- lm(mpg ~ wt + factor(cyl), data = mtcars)
  out <- check_collinearity(m, verbose = FALSE)
  expect_true(all(is.finite(out$VIF_CI_low)))
  expect_true(all(is.finite(out$VIF_CI_high)))
  expect_true(all(out$VIF_CI_low <= out$VIF_CI_high))
})

test_that(".pf_ncp approximation for a large noncentrality", {
  # reference values from MBESS::ci.R2() for VIF 1000 at n = 300 and k = 3
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
  expect_equal(
    1 / (1 - c(out$CI_low[1], out$CI_high[1])),
    c(834.1221966, 1151.8537036),
    tolerance = 1e-5
  )
})

test_that(".vif_ci handles infinite VIFs and failed integration", {
  m <- lm(mpg ~ wt + hp, data = mtcars)
  out <- .vif_ci(m, c(Inf, 2), ci = 0.95, n_coef = c(1, 1))
  expect_identical(out$CI_low[1], 1)
  expect_identical(out$CI_high[1], 1)

  d <- data.frame(y = c(1, 3, 2), a = c(1, 2, 3), b = c(2, 1, 4))
  m <- lm(y ~ a + b, data = d)
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
# intercept
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
  group_effect <- stats::rnorm(25, sd = 0.5)[d$g]
  d$o <- cut(
    d$x1 + d$x2 + group_effect + stats::rnorm(n),
    breaks = 4,
    ordered_result = TRUE,
    labels = c("a", "b", "c", "d")
  )
  d
}

test_that("check_collinearity, polr and svyolr match car::vif", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("survey")
  skip_if_not_installed("car")
  d <- .ordinal_vif_data()
  des <- survey::svydesign(ids = ~1, data = d, weights = rep(1, nrow(d)))
  models <- list(
    polr = MASS::polr(o ~ x1 + f + x2 + x3, data = d, Hess = TRUE),
    svyolr = survey::svyolr(o ~ x1 + f + x2 + x3, design = des)
  )
  for (m in models) {
    out <- check_collinearity(m)
    expect_identical(out$Term, c("x1", "f", "x2", "x3"))
    expect_equal(out$VIF, car::vif(m)[, "GVIF"], tolerance = 1e-6, ignore_attr = TRUE)
  }
})

test_that("check_collinearity, clm2 and clmm2 match clm and clmm", {
  skip_if_not_installed("ordinal")
  skip_if_not_installed("withr")
  withr::local_package("ordinal")
  d <- .ordinal_vif_data()
  ref <- check_collinearity(ordinal::clm(o ~ x3 + f + x1 + x2, data = d))
  out <- check_collinearity(ordinal::clm2(o ~ x3 + f + x1 + x2, data = d, Hess = TRUE))
  expect_identical(out$Term, ref$Term)
  expect_equal(out$VIF, ref$VIF, tolerance = 1e-4)

  ref <- check_collinearity(ordinal::clmm(o ~ x1 + f + x2 + x3 + (1 | g), data = d))
  out <- check_collinearity(
    ordinal::clmm2(o ~ x1 + f + x2 + x3, random = g, data = d, Hess = TRUE)
  )
  expect_identical(out$Term, c("x1", "f", "x2", "x3"))
  expect_equal(out$VIF, ref$VIF, tolerance = 1e-4)
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
    expect_warning(
      out <- check_collinearity(models[[i]]),
      "x4"
    )
    expect_identical(out$Term, c("x1", "x2", "x3"))
    expect_equal(out$VIF, ref, tolerance = tolerances[[i]], ignore_attr = TRUE)
  }
})

test_that("check_collinearity, ordinal models with backticks, unused levels, or other contrasts", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("ordinal")
  skip_if_not_installed("car")
  d <- .ordinal_vif_data()

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

  ref <- check_collinearity(ordinal::clm(o ~ x1 + f + x2 + x3, data = d))
  out <- check_collinearity(ordinal::clm(
    o ~ x1 + f + x2 + x3,
    data = d,
    contrasts = list(f = "contr.sum")
  ))
  expect_identical(out$Term, ref$Term)
  expect_equal(out$VIF, ref$VIF, tolerance = 1e-4)
})

# `assign` maps each column of `v` to its model term
.gvif_direct <- function(v, assign) {
  .gvif_blocks(v, split(seq_along(assign), factor(assign, unique(assign))))
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
  assign <- attr(stats::model.matrix(~ x2 + x1 + f, data = d), "assign")[-1]
  m <- glmmTMB::glmmTMB(y ~ x1 + f, ziformula = ~ x2 + x1 + f, data = d, family = poisson())
  out <- check_collinearity(m, component = "zero_inflated")
  expect_identical(out$Term, c("x2", "x1", "f"))
  expect_equal(out$VIF, .gvif_direct(vcov(m)$zi[-1, -1], assign), tolerance = 1e-8)
})

test_that("check_collinearity, glmmTMB zero-inflation component with one term", {
  skip_if_not_installed("glmmTMB")
  d <- .zi_test_data()
  m <- glmmTMB::glmmTMB(y ~ x1 + f, ziformula = ~x2, data = d, family = poisson())
  expect_message(
    out <- check_collinearity(m, component = "all"),
    "Not enough model terms in the zero-inflat"
  )
  expect_identical(out$Term, c("x1", "f"))
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
})

test_that("check_collinearity, zero-inflation and conditional parts with different intercepts", {
  skip_if_not_installed("glmmTMB")
  d <- .zi_test_data()
  assign_no_int <- attr(stats::model.matrix(~ 0 + x2 + x1 + f, data = d), "assign")
  assign_int <- attr(stats::model.matrix(~ x2 + x1 + f, data = d), "assign")[-1]

  m <- glmmTMB::glmmTMB(y ~ x1, ziformula = ~ 0 + x2 + x1 + f, data = d, family = poisson())
  expect_message(
    out <- check_collinearity(m, component = "zero_inflated"),
    "Model without intercept"
  )
  expect_identical(out$Term, c("x2", "x1", "f"))
  expect_equal(out$VIF, .gvif_direct(vcov(m)$zi, assign_no_int), tolerance = 1e-8)

  m <- glmmTMB::glmmTMB(y ~ 0 + x1 + x2, ziformula = ~ x2 + x1 + f, data = d, family = poisson())
  expect_no_message(out <- check_collinearity(m, component = "zero_inflated"))
  expect_identical(out$Term, c("x2", "x1", "f"))
  expect_equal(out$VIF, .gvif_direct(vcov(m)$zi[-1, -1], assign_int), tolerance = 1e-8)
})

test_that("check_collinearity, pscl models with a message and the default verbose", {
  skip_if_not_installed("pscl")
  d <- .zi_test_data()
  models <- list(
    pscl::zeroinfl(y ~ x1 | x2 + x1, data = d),
    pscl::hurdle(y ~ x1 | x2 + x1, data = d)
  )
  for (m in models) {
    expect_message(
      out <- check_collinearity(m),
      "Not enough model terms in the conditional part"
    )
    expect_identical(out$Term, c("x2", "x1"))
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
