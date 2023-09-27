test_that("logLik", {
  skip_if_not_installed("plm")
  skip_if_not_installed("withr")
  
  withr::local_options(list(expressions = 25))
  set.seed(1)
  nnn <- 100
  ddta <- data.frame(
    x1 = rnorm(nnn),
    x2 = rnorm(nnn),
    id = rep_len(1:(nnn / 10), nnn),
    year = rep_len(1:11, nnn)
  )
  ddta$y <- ddta$x1 * 0.5 - ddta$x2 * 0.5 + rnorm(nnn)

  m1 <- lm(y ~ x1 + x2, data = ddta)
  l1 <- logLik(m1)

  m2 <- plm(
    y ~ x1 + x2,
    data = ddta,
    model = "pooling",
    index = c("id", "year")
  )
  l2 <- logLik(m2)
  expect_equal(l1, l2, tolerance = 1e-3, ignore_attr = TRUE)
  options(expressions = expr)
})
