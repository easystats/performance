skip_if_not_installed("DHARMa", minimum_version = "0.5.0")

test_that("check_overdispersion, glmmTMB-poisson", {
  skip_if_not_installed("glmmTMB")
  skip_if_not(getRversion() >= "4.0.0")
  data(Salamanders, package = "glmmTMB")

  m1 <- glm(count ~ spp + mined, family = poisson, data = Salamanders)
  out <- check_overdispersion(m1)
  expect_equal(
    out,
    structure(
      list(
        chisq_statistic = 1873.71012423995,
        dispersion_ratio = 2.94608510100621,
        residual_df = 636L,
        p_value = 3.26607509162498e-122
      ),
      class = c("check_overdisp", "see_check_overdisp"),
      object_name = "m1"
    ),
    tolerance = 1e-3
  )
  expect_identical(
    capture.output(print(out)),
    c(
      "# Overdispersion test",
      "",
      "       dispersion ratio =    2.946",
      "  Pearson's Chi-Squared = 1873.710",
      "                p-value =  < 0.001",
      ""
    )
  )
  expect_message(capture.output(print(out)), "Overdispersion detected")

  set.seed(123)
  out <- check_overdispersion(simulate_residuals(m1))
  expect_equal(
    out,
    structure(
      list(
        dispersion_ratio = 3.91516791651235,
        p_value = 0
      ),
      class = c("check_overdisp", "see_check_overdisp"),
      simulated = TRUE
    ),
    tolerance = 1e-3
  )
})


test_that("check_overdispersion, glmmTMB-poisson mixed", {
  skip_if_not_installed("glmmTMB")
  skip_if_not(getRversion() >= "4.0.0")
  skip_if_not_installed("DHARMa", minimum_version = "0.5.0")

  data(Salamanders, package = "glmmTMB")

  m2 <- glmmTMB::glmmTMB(
    count ~ mined + spp + (1 | site),
    family = poisson,
    data = Salamanders
  )
  set.seed(123)
  expect_equal(
    check_overdispersion(m2, residual_type = "normal"),
    structure(
      list(
        chisq_statistic = 1475.87512547128,
        dispersion_ratio = 2.32421279601777,
        residual_df = 635L,
        p_value = 8.41489530177729e-69
      ),
      class = c("check_overdisp", "see_check_overdisp"),
      object_name = "m2"
    ),
    tolerance = 1e-3
  )
  set.seed(123)
  expect_equal(
    check_overdispersion(m2),
    structure(
      list(dispersion_ratio = 3.04011005020607, p_value = 0),
      class = c("check_overdisp", "see_check_overdisp"),
      object_name = "m2",
      simulated = TRUE
    ),
    tolerance = 1e-3
  )
})


test_that("check_overdispersion, zero-inflated and negbin", {
  skip_if_not_installed("glmmTMB")
  skip_if_not(getRversion() >= "4.0.0")
  data(Salamanders, package = "glmmTMB")

  m1 <- glmmTMB::glmmTMB(
    count ~ spp + mined,
    ziformula = ~ spp + mined,
    family = poisson,
    data = Salamanders
  )
  m2 <- glmmTMB::glmmTMB(
    count ~ spp + mined,
    family = poisson,
    data = Salamanders
  )
  m3 <- glmmTMB::glmmTMB(
    count ~ spp + mined,
    family = glmmTMB::nbinom1(),
    data = Salamanders
  )

  set.seed(123)
  expect_equal(
    check_overdispersion(m1),
    structure(
      list(
        dispersion_ratio = 1.98057695890769,
        p_value = 0
      ),
      class = c("check_overdisp", "see_check_overdisp"),
      simulated = TRUE
    ),
    tolerance = 1e-4,
    ignore_attr = TRUE
  )

  set.seed(123)
  expect_equal(
    check_overdispersion(m1, residual_type = "normal"),
    structure(
      list(
        chisq_statistic = 1027.18155565511,
        dispersion_ratio = 1.63563941983298,
        residual_df = 628L,
        p_value = 9.12825627359497e-22
      ),
      class = c("check_overdisp", "see_check_overdisp"),
      object_name = "m1"
    ),
    tolerance = 1e-4,
    ignore_attr = TRUE
  )

  set.seed(123)
  expect_equal(
    check_overdispersion(m2, residual_type = "normal"),
    structure(
      list(
        chisq_statistic = 1873.7105986433,
        dispersion_ratio = 2.94608584692342,
        residual_df = 636L,
        p_value = 3.26556213101505e-122
      ),
      class = c("check_overdisp", "see_check_overdisp"),
      object_name = "m1"
    ),
    tolerance = 1e-4,
    ignore_attr = TRUE
  )
  set.seed(123)
  expect_equal(
    check_overdispersion(m2),
    structure(
      list(dispersion_ratio = 3.91516799008681, p_value = 0),
      class = c("check_overdisp", "see_check_overdisp"),
      object_name = "m2",
      simulated = TRUE
    ),
    tolerance = 1e-4,
    ignore_attr = TRUE
  )

  set.seed(123)
  expect_equal(
    check_overdispersion(m3, residual_type = "normal"),
    structure(
      list(
        chisq_statistic = 544.29641690291,
        dispersion_ratio = 0.857159711658125,
        residual_df = 635L,
        p_value = 0.996077528983478
      ),
      class = c("check_overdisp", "see_check_overdisp"),
      object_name = "m3"
    ),
    tolerance = 1e-4,
    ignore_attr = TRUE
  )
  set.seed(123)
  expect_equal(
    check_overdispersion(m3),
    structure(
      list(dispersion_ratio = 1.18027855021855, p_value = 0.232),
      class = c("check_overdisp", "see_check_overdisp"),
      object_name = "m3",
      simulated = TRUE
    ),
    tolerance = 1e-4,
    ignore_attr = TRUE
  )
})


test_that("check_overdispersion, MASS::negbin", {
  skip_if_not_installed("MASS")
  set.seed(3)
  mu <- rpois(500, lambda = 3)
  x <- rnorm(500, mu, mu * 3)
  x <- ceiling(x)
  x <- pmax(x, 0)
  m <- MASS::glm.nb(x ~ mu)
  out <- check_overdispersion(m)
  expect_equal(
    out,
    structure(
      list(
        dispersion_ratio = 0.409521313173506,
        p_value = 0
      ),
      class = c("check_overdisp", "see_check_overdisp")
    ),
    ignore_attr = TRUE,
    tolerance = 1e-4
  )
  expect_identical(
    capture.output(print(out)),
    c(
      "# Overdispersion test (using simulated residuals)",
      "",
      " dispersion ratio =   0.410",
      "          p-value = < 0.001",
      ""
    )
  )
  expect_message(capture.output(print(out)), "Underdispersion detected")

  # check that plot works
  skip_if_not_installed("see", minimum_version = "0.9.1")
  expect_s3_class(plot(out), "ggplot")
})


test_that("check_overdispersion, genpois", {
  skip_if_not_installed("glmmTMB")
  skip_if_not(getRversion() >= "4.0.0")
  data(Salamanders, package = "glmmTMB")

  model <- glmmTMB::glmmTMB(
    count ~ mined + spp + (1 | site),
    family = glmmTMB::genpois(),
    data = Salamanders
  )
  set.seed(123)
  expect_equal(
    check_overdispersion(model),
    structure(
      list(dispersion_ratio = 1.13005481966618, p_value = 0.408),
      class = c("check_overdisp", "see_check_overdisp"),
      object_name = "model",
      simulated = TRUE
    ),
    tolerance = 1e-4
  )
  set.seed(123)
  expect_equal(
    check_overdispersion(model, residual_type = "normal"),
    structure(
      list(
        chisq_statistic = 473.48007461303,
        dispersion_ratio = 0.74681399781235,
        residual_df = 634L,
        p_value = 0.999999604566096
      ),
      class = c("check_overdisp", "see_check_overdisp"),
      object_name = "model"
    ),
    tolerance = 1e-4
  )
})


test_that("check_overdispersion, glm can use normal residuals", {
  #fmt: skip
  dat <- data.frame(
    n = c(
      55, 59, 74, 7, 54, 54, 57, 48, 55, 57, 41,
      20, 13, 21, 13, 32, 38, 42, 37, 14, 19
    ),
    success = c(
      26, 35, 28, 6, 16, 35, 28, 21, 31, 10, 2, 9, 7,
      18, 1, 26, 28, 3, 17, 11, 17
    ),
    x = c(
      3.464, 1.599, 3.39, 3.047, 2.442, 1.777, 3.363,
      4.626, 2.701, 4.636, 3.622, 2.031, 1.666, 2.218, 3.338, 4.255,
      2.476, 4.727, 3.317, 3.925, 2.854
    )
  )

  mod1 <- glm(cbind(success, n - success) ~ x, data = dat, family = binomial)

  set.seed(123)
  out <- check_overdispersion(mod1, residual_type = "normal")
  expect_equal(
    out,
    structure(
      list(
        chisq_statistic = 137.09656611108,
        dispersion_ratio = 7.21560874268842,
        residual_df = 19L,
        p_value = 6.54224660869874e-20
      ),
      class = c("check_overdisp", "see_check_overdisp"),
      object_name = "mod1"
    ),
    tolerance = 1e-4
  )

  set.seed(123)
  out <- check_overdispersion(mod1)
  expect_equal(
    out,
    structure(
      list(dispersion_ratio = 1.07771876659311, p_value = 0.528),
      class = c("check_overdisp", "see_check_overdisp"),
      object_name = "mod1",
      simulated = TRUE
    ),
    tolerance = 1e-4
  )
})


test_that("check_overdispersion, glm binomial with cbind() uses Pearson residuals", {
  #fmt: skip
  dat <- data.frame(
    n = c(
      55, 59, 74, 7, 54, 54, 57, 48, 55, 57, 41,
      20, 13, 21, 13, 32, 38, 42, 37, 14, 19
    ),
    success = c(
      26, 35, 28, 6, 16, 35, 28, 21, 31, 10, 2, 9, 7,
      18, 1, 26, 28, 3, 17, 11, 17
    ),
    x = c(
      3.464, 1.599, 3.39, 3.047, 2.442, 1.777, 3.363,
      4.626, 2.701, 4.636, 3.622, 2.031, 1.666, 2.218, 3.338, 4.255,
      2.476, 4.727, 3.317, 3.925, 2.854
    )
  )
  mod1 <- glm(cbind(success, n - success) ~ x, data = dat, family = binomial)
  m_binom <- glm(cbind(ncases, ncontrols) ~ agegp, family = binomial, data = esoph)

  for (fit in list(mod1, m_binom)) {
    out <- check_overdispersion(fit, residual_type = "normal")
    pearson_chisq <- sum(stats::residuals(fit, type = "pearson")^2)
    expect_equal(out$chisq_statistic, pearson_chisq, tolerance = 1e-6)
    expect_equal(
      out$dispersion_ratio,
      pearson_chisq / stats::df.residual(fit),
      tolerance = 1e-6
    )
  }
})


test_that("check_overdispersion, quasi families warn", {
  m_quasipois <- glm(
    DriversKilled ~ PetrolPrice,
    family = quasipoisson,
    data = Seatbelts
  )
  m_quasibinom <- glm(
    cbind(ncases, ncontrols) ~ agegp,
    family = quasibinomial,
    data = esoph
  )

  for (fit in list(m_quasipois, m_quasibinom)) {
    fam <- stats::family(fit)$family
    for (rt in list(NULL, "simulated")) {
      warns <- character(0)
      withCallingHandlers(
        check_overdispersion(fit, residual_type = rt),
        warning = function(w) {
          warns <<- c(warns, gsub("\\s+", " ", conditionMessage(w)))
          invokeRestart("muffleWarning")
        }
      )
      expect_length(warns, 1)
      expect_match(warns, fam, fixed = TRUE)
      expect_match(warns, "already estimates a dispersion parameter", fixed = TRUE)
      expect_match(warns, "Simulated residuals are not available", fixed = TRUE)
    }
    expect_no_warning(check_overdispersion(fit, verbose = FALSE))
    expect_no_warning(
      check_overdispersion(fit, residual_type = "simulated", verbose = FALSE)
    )
  }
})


test_that("check_overdispersion, quasi families return the unscaled Pearson test", {
  m_pois <- glm(DriversKilled ~ PetrolPrice, family = poisson, data = Seatbelts)
  m_quasipois <- glm(
    DriversKilled ~ PetrolPrice,
    family = quasipoisson,
    data = Seatbelts
  )
  m_binom <- glm(cbind(ncases, ncontrols) ~ agegp, family = binomial, data = esoph)
  m_quasibinom <- glm(
    cbind(ncases, ncontrols) ~ agegp,
    family = quasibinomial,
    data = esoph
  )
  stats_names <- c("chisq_statistic", "dispersion_ratio", "residual_df", "p_value")

  out_pois <- check_overdispersion(m_pois)
  out_quasipois <- check_overdispersion(m_quasipois, verbose = FALSE)
  expect_equal(unclass(out_quasipois)[stats_names], unclass(out_pois)[stats_names])

  out_binom <- check_overdispersion(m_binom, residual_type = "normal")
  out_quasibinom <- check_overdispersion(m_quasibinom, verbose = FALSE)
  expect_equal(unclass(out_quasibinom)[stats_names], unclass(out_binom)[stats_names])
})


test_that("check_overdispersion, print notes quasi families", {
  m_pois <- glm(DriversKilled ~ PetrolPrice, family = poisson, data = Seatbelts)
  m_quasipois <- glm(
    DriversKilled ~ PetrolPrice,
    family = quasipoisson,
    data = Seatbelts
  )

  expect_no_warning(check_overdispersion(m_pois))
  out_pois <- check_overdispersion(m_pois)
  msgs <- character(0)
  withCallingHandlers(
    capture.output(print(out_pois)),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  expect_identical(msgs, "Overdispersion detected.\n")

  out_quasipois <- check_overdispersion(m_quasipois, verbose = FALSE)
  msgs <- character(0)
  withCallingHandlers(
    capture.output(print(out_quasipois)),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  expect_length(msgs, 1)
  expect_match(msgs, "Overdispersion detected\\..*dispersion")
})


test_that("check_overdispersion, fixest quasipoisson warns", {
  skip_if_not_installed("fixest")
  m_fixest <- fixest::feglm(
    DriversKilled ~ PetrolPrice,
    data = as.data.frame(Seatbelts),
    family = "quasipoisson"
  )
  warns <- character(0)
  withCallingHandlers(
    check_overdispersion(m_fixest),
    warning = function(w) {
      warns <<- c(warns, gsub("\\s+", " ", conditionMessage(w)))
      invokeRestart("muffleWarning")
    }
  )
  expect_length(warns, 1)
  expect_match(warns, "already estimates a dispersion parameter", fixed = TRUE)
  expect_no_warning(check_overdispersion(m_fixest, verbose = FALSE))

  m_fixest_multi <- fixest::feglm(
    c(DriversKilled, VanKilled) ~ PetrolPrice,
    data = as.data.frame(Seatbelts),
    family = "quasipoisson"
  )
  expect_no_warning(check_overdispersion(m_fixest_multi, verbose = FALSE))
})


test_that("check_overdispersion, glm with na.exclude", {
  d <- as.data.frame(Seatbelts)
  d$PetrolPrice[c(3, 50, 120)] <- NA
  m_pois <- glm(
    DriversKilled ~ PetrolPrice,
    family = poisson,
    data = d,
    na.action = na.exclude
  )
  out <- check_overdispersion(m_pois)
  pearson_chisq <- sum(stats::residuals(m_pois, type = "pearson")^2, na.rm = TRUE)
  expect_equal(out$chisq_statistic, pearson_chisq, tolerance = 1e-6)
  expect_identical(out$residual_df, stats::df.residual(m_pois))
  expect_message(capture.output(print(out)), "Overdispersion detected")
})


test_that("check_overdispersion, glm poisson with prior weights", {
  d <- as.data.frame(Seatbelts)
  d$w <- rep(1:3, length.out = nrow(d))
  m_pois <- glm(DriversKilled ~ PetrolPrice, family = poisson, data = d, weights = w)
  out <- check_overdispersion(m_pois)
  pearson_chisq <- sum(stats::residuals(m_pois, type = "pearson")^2)
  expect_equal(out$chisq_statistic, pearson_chisq, tolerance = 1e-6)
  expect_equal(
    out$dispersion_ratio,
    pearson_chisq / stats::df.residual(m_pois),
    tolerance = 1e-6
  )
})


test_that("check_overdispersion, glm uses df.residual() with aliased coefficients", {
  d_pois <- as.data.frame(Seatbelts)
  d_pois$PetrolPrice2 <- 2 * d_pois$PetrolPrice
  d_binom <- esoph
  d_binom$alc <- as.numeric(d_binom$alcgp)
  d_binom$alc2 <- 2 * d_binom$alc

  fits <- list(
    poisson = glm(DriversKilled ~ PetrolPrice + PetrolPrice2, poisson, d_pois),
    quasipoisson = glm(DriversKilled ~ PetrolPrice + PetrolPrice2, quasipoisson, d_pois),
    binomial = glm(cbind(ncases, ncontrols) ~ alc + alc2, binomial, d_binom),
    quasibinomial = glm(cbind(ncases, ncontrols) ~ alc + alc2, quasibinomial, d_binom)
  )

  for (fam in names(fits)) {
    fit <- fits[[fam]]
    # the aliased coefficient makes the parameter count differ from df.residual()
    expect_true(anyNA(stats::coef(fit)))
    out <- check_overdispersion(fit, residual_type = "normal", verbose = FALSE)
    rdf <- stats::df.residual(fit)
    expect_identical(out$residual_df, rdf)
    expect_equal(out$dispersion_ratio, out$chisq_statistic / rdf, tolerance = 1e-10)
    expect_equal(
      out$p_value,
      stats::pchisq(out$chisq_statistic, rdf, lower.tail = FALSE),
      tolerance = 1e-10
    )
    if (startsWith(fam, "quasi")) {
      expect_equal(out$dispersion_ratio, summary(fit)$dispersion, tolerance = 1e-6)
    }
  }
})


test_that("check_overdispersion, fixest uses df.residual() with fixed effects", {
  skip_if_not_installed("fixest")
  set.seed(2)
  d <- data.frame(x = rnorm(150), f = rep(1:10, 15))
  d$y <- rpois(150, exp(1 + 0.3 * d$x + d$f / 10))

  m_pois <- fixest::fepois(y ~ x | f, data = d)
  out <- check_overdispersion(m_pois)
  rdf <- stats::df.residual(m_pois)
  # the fixed effects make the parameter count differ from df.residual()
  expect_lt(rdf, stats::nobs(m_pois) - 1)
  expect_identical(out$residual_df, rdf)
  expect_equal(out$dispersion_ratio, out$chisq_statistic / rdf, tolerance = 1e-10)
  expect_equal(
    out$p_value,
    stats::pchisq(out$chisq_statistic, rdf, lower.tail = FALSE),
    tolerance = 1e-10
  )

  m_quasi <- fixest::feglm(y ~ x | f, data = d, family = "quasipoisson")
  out <- check_overdispersion(m_quasi, verbose = FALSE)
  expect_identical(out$residual_df, stats::df.residual(m_quasi))
  expect_equal(out$dispersion_ratio, m_quasi$dispersion, tolerance = 1e-4)
})


test_that("check_overdispersion, fixest_multi from split matches each sample", {
  skip_if_not_installed("fixest")
  set.seed(2)
  d <- data.frame(
    x = rnorm(150),
    f = rep(1:10, 15),
    g = rep(c("a", "b"), c(100, 50))
  )
  d$y <- rpois(150, exp(1 + 0.3 * d$x))

  m_multi <- fixest::fepois(y ~ x | f, data = d, split = ~g)
  expect_no_warning({
    out <- check_overdispersion(m_multi)
  })
  expect_length(out, 2)

  stats_names <- c("chisq_statistic", "dispersion_ratio", "residual_df", "p_value")
  for (i in seq_along(out)) {
    grp <- c("a", "b")[i]
    m_single <- fixest::fepois(y ~ x | f, data = d[d$g == grp, ])
    expect_equal(
      unclass(out[[i]])[stats_names],
      unclass(check_overdispersion(m_single))[stats_names],
      tolerance = 1e-10
    )
  }
})


test_that("check_overdispersion, glmx uses Pearson residuals of the fitted family", {
  skip_if_not_installed("glmx")
  skip_if_not_installed("MASS")

  # negative binomial, data from the glmx help page
  set.seed(1)
  d <- data.frame(x = stats::runif(200, -1, 1))
  d$y <- stats::rnbinom(200, mu = exp(0 + 3 * d$x), size = 1)
  m_nb <- glmx::glmx(
    y ~ x,
    data = d,
    family = MASS::negative.binomial,
    xlink = "log",
    xstart = 0
  )
  m_ref <- MASS::glm.nb(y ~ x, data = d)
  out <- check_overdispersion(m_nb)
  expect_equal(
    out$chisq_statistic,
    sum(stats::residuals(m_ref, type = "pearson")^2),
    tolerance = 1e-3
  )
  expect_equal(out$residual_df, stats::nobs(m_nb) - m_nb$df)

  # binomial with a cbind() response, unweighted and weighted
  set.seed(4)
  d <- data.frame(x = stats::rnorm(150))
  d$s <- stats::rbinom(150, 10, stats::plogis(0.3 + d$x))
  d$f <- 10 - d$s
  d$pw <- rep(1:3, length.out = 150)
  fam <- function(shape) stats::binomial(link = glmx::gosset(shape))
  for (weighted in c(FALSE, TRUE)) {
    if (weighted) {
      m_binom <- glmx::glmx(
        cbind(s, f) ~ x,
        data = d,
        family = fam,
        xlink = "log",
        xstart = 0,
        weights = pw
      )
      m_ref <- glm(cbind(s, f) ~ x, data = d, family = m_binom$family$glm, weights = pw)
    } else {
      m_binom <- glmx::glmx(
        cbind(s, f) ~ x,
        data = d,
        family = fam,
        xlink = "log",
        xstart = 0
      )
      m_ref <- glm(cbind(s, f) ~ x, data = d, family = m_binom$family$glm)
    }
    out <- check_overdispersion(m_binom)
    expect_equal(
      out$chisq_statistic,
      sum(stats::residuals(m_ref, type = "pearson")^2),
      tolerance = 1e-8
    )
  }

  # other families have no Pearson residuals here
  set.seed(1)
  d <- data.frame(x = stats::runif(200, -1, 1))
  d$y <- stats::rpois(200, exp(1 + d$x))
  m_pois <- glmx::glmx(
    y ~ x,
    data = d,
    family = function(p) stats::poisson(link = stats::power(p)),
    xlink = "identity",
    xstart = 0.5
  )
  expect_error(
    check_overdispersion(m_pois),
    "only available for\\s+negative\\s+binomial\\s+and\\s+binomial\\s+families"
  )
})
