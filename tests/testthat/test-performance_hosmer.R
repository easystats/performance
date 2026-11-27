test_that("performance_hosmer", {
  m <- glm(vs ~ wt + mpg, data = mtcars, family = "binomial")
  out <- performance_hosmer(m)
  expect_equal(out$chisq, 5.13687236291332, tolerance = 1e-10)
  expect_equal(out$df, 8, tolerance = 1e-10)
  expect_equal(out$p.value, 0.742851895405543, tolerance = 1e-10)
})


test_that("performance_hosmer, tied fitted values", {
  # see #578
  d <- as.data.frame(Titanic)
  d <- d[rep(seq_len(nrow(d)), d$Freq), 1:4]
  m <- glm(Survived ~ ., data = d, family = binomial())
  expect_message(performance_hosmer(m), "6 groups")
  expect_silent(performance_hosmer(m, verbose = FALSE))
  out <- performance_hosmer(m, verbose = FALSE)
  # value from groups cut at c(-Inf, unique(q[-c(1, 11)]), Inf),
  # with q <- quantile(fitted(m), 0:10 / 10)
  expect_equal(out$chisq, 36.1857138095, tolerance = 1e-8)
  expect_identical(out$df, 4)
  expect_identical(nrow(attr(out, "observed")), 6L)
  expect_identical(nrow(attr(out, "expected")), 6L)
  expect_identical(sum(attr(out, "observed")), as.numeric(nrow(model.frame(m))))
  expect_equal(sum(attr(out, "expected")), nrow(model.frame(m)), tolerance = 1e-10)

  m <- glm(am ~ vs, data = mtcars, family = binomial)
  expect_no_message(
    expect_error(performance_hosmer(m), "fewer than 3 groups")
  )
})


test_that("performance_hosmer, aggregated binomial response", {
  set.seed(123)
  d <- data.frame(x = rnorm(40), n = sample(5:15, 40, replace = TRUE))
  d$y <- rbinom(40, d$n, stats::plogis(0.5 + d$x))
  d_long <- d[rep(seq_len(nrow(d)), d$n), ]
  d_long$y <- unlist(lapply(seq_len(nrow(d)), function(i) {
    rep(c(1, 0), c(d$y[i], d$n[i] - d$y[i]))
  }))
  m_long <- glm(y ~ x, data = d_long, family = binomial)
  out_long <- performance_hosmer(m_long, verbose = FALSE)

  m <- glm(cbind(y, n - y) ~ x, data = d, family = binomial)
  out <- performance_hosmer(m, verbose = FALSE)
  expect_equal(out$chisq, out_long$chisq, tolerance = 1e-8)
  expect_identical(out$df, out_long$df)
  expect_equal(attr(out, "observed"), attr(out_long, "observed"), tolerance = 1e-8)

  m <- glm(y / n ~ x, data = d, weights = n, family = binomial)
  out <- performance_hosmer(m, verbose = FALSE)
  expect_equal(out$chisq, out_long$chisq, tolerance = 1e-8)

  skip_if_not_installed("lme4")
  data(cbpp, package = "lme4")
  cbpp_long <- cbpp[rep(seq_len(nrow(cbpp)), cbpp$size), ]
  cbpp_long$y <- unlist(lapply(seq_len(nrow(cbpp)), function(i) {
    rep(c(1, 0), c(cbpp$incidence[i], cbpp$size[i] - cbpp$incidence[i]))
  }))
  m_long <- lme4::glmer(y ~ period + (1 | herd), data = cbpp_long, family = binomial)
  m <- lme4::glmer(
    cbind(incidence, size - incidence) ~ period + (1 | herd),
    data = cbpp,
    family = binomial
  )
  out_long <- performance_hosmer(m_long, verbose = FALSE)
  out <- performance_hosmer(m, verbose = FALSE)
  expect_equal(out$chisq, out_long$chisq, tolerance = 1e-4)
  expect_identical(sum(attr(out, "observed")), sum(cbpp$size))
})
