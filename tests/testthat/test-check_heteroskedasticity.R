test_that("check_heteroskedasticity, lm", {
  data(mtcars)
  m <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
  out <- check_heteroscedasticity(m)
  expect_equal(as.vector(out), 0.0423, ignore_attr = TRUE, tolerance = 1e-2)
  expect_identical(
    capture.output(print(out)),
    "Warning: Heteroscedasticity (non-constant error variance) detected (p = 0.042)."
  )
  m <- lm(mpg ~ hp, data = mtcars)
  out <- check_heteroscedasticity(m)
  expect_equal(as.vector(out), 0.8271352, ignore_attr = TRUE, tolerance = 1e-2)
  expect_identical(
    capture.output(print(out)),
    "OK: Error variance appears to be homoscedastic (p = 0.827)."
  )
})

test_that("check_heteroskedasticity, hlm", {
  set.seed(1)
  n <- 600
  size <- 20
  x <- runif(n, -3, 3)
  d <- data.frame(x = x, y = rbinom(n, size, plogis(-0.5 + 1.2 * x)))
  d$f <- size - d$y

  m <- glm(cbind(y, f) ~ x, family = binomial, data = d)
  expect_message(
    {
      out <- check_heteroscedasticity(m)
    },
    regex = "There is only a `plot()` method",
    fixed = TRUE
  )

  skip_if_not_installed("glmmTMB")
  m <- glmmTMB::glmmTMB(cbind(y, f) ~ x, family = binomial, data = d)
  expect_message(
    {
      out <- check_heteroscedasticity(m)
    },
    regex = "There is only a `plot()` method",
    fixed = TRUE
  )
})

test_that("check_heteroskedasticity, count model", {
  set.seed(1)
  n <- 100
  x <- runif(n, -3, 3)
  d <- data.frame(
    x = x,
    g = factor(rep(LETTERS[1:10], each = n / 10)),
    y = rpois(n, exp(0.5 + 0.3 * x))
  )

  skip_if_not_installed("lme4")
  m <- lme4::glmer(y ~ x + (1 | g), family = poisson, data = d)
  expect_message(
    {
      out <- check_heteroscedasticity(m)
    },
    regex = "check your model for overdispersion",
    fixed = TRUE
  )
})

test_that("check_heteroskedasticity, same result in any units of the response", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("glmmTMB")
  skip_if_not_installed("nlme")
  set.seed(123)
  d <- data.frame(g = factor(rep(1:30, each = 10)), x = rnorm(300))
  d$y <- 2 + 0.5 * d$x + rnorm(30)[d$g] + rnorm(300)
  d$y100 <- d$y * 100

  p_lmer <- as.vector(check_heteroscedasticity(lme4::lmer(y ~ x + (1 | g), data = d)))
  p_lmer100 <- as.vector(check_heteroscedasticity(lme4::lmer(y100 ~ x + (1 | g), data = d)))
  expect_equal(p_lmer100, p_lmer, tolerance = 1e-6)

  p_tmb <- as.vector(check_heteroscedasticity(glmmTMB::glmmTMB(y ~ x + (1 | g), data = d)))
  p_tmb100 <- as.vector(check_heteroscedasticity(glmmTMB::glmmTMB(y100 ~ x + (1 | g), data = d)))
  expect_equal(p_tmb100, p_tmb, tolerance = 1e-4)

  p_lme <- as.vector(check_heteroscedasticity(nlme::lme(y ~ x, random = ~ 1 | g, data = d)))
  p_lme100 <- as.vector(check_heteroscedasticity(nlme::lme(y100 ~ x, random = ~ 1 | g, data = d)))
  expect_equal(p_lme100, p_lme, tolerance = 1e-6)
  # lmer() and lme() fit the same model here
  expect_equal(p_lme, p_lmer, tolerance = 1e-4)

  p_gls <- as.vector(check_heteroscedasticity(nlme::gls(y ~ x, data = d)))
  p_gls100 <- as.vector(check_heteroscedasticity(nlme::gls(y100 ~ x, data = d)))
  expect_equal(p_gls100, p_gls, tolerance = 1e-6)
})
