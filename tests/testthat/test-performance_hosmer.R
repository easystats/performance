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
