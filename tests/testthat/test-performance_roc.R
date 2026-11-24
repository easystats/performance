skip_if_not_installed("bayestestR")

test_that("performance_roc-mixed-1", {
  skip_if_not_installed("lme4")
  m <- lme4::glmer(vs ~ mpg + (1 | gear), family = "binomial", data = mtcars)
  roc <- performance_roc(m)
  # fmt: skip
  expect_equal(
    roc$Sensitivity,
    c(
      0, 0.07143, 0.14286, 0.21429, 0.28571, 0.35714, 0.42857, 0.57143,
      0.57143, 0.64286, 0.64286, 0.71429, 0.78571, 0.85714, 0.85714,
      0.92857, 0.92857, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
    ),
    tolerance = 1e-3
  )
})


test_that("performance_roc-2", {
  set.seed(123)
  d <- iris[sample.int(nrow(iris), size = 50), ]
  d$y <- as.factor(rbinom(nrow(d), size = 1, 0.3))
  dat <<- d

  m <- glm(
    y ~ Sepal.Length + Sepal.Width,
    data = dat,
    family = "binomial"
  )
  roc <- performance_roc(m)
  # fmt: skip
  expect_equal(
    roc$Sensitivity,
    c(
      0, 0, 0.07692, 0.07692, 0.07692, 0.15385, 0.23077, 0.23077,
      0.23077, 0.23077, 0.30769, 0.30769, 0.30769, 0.30769, 0.30769,
      0.38462, 0.38462, 0.46154, 0.46154, 0.53846, 0.53846, 0.53846,
      0.61538, 0.61538, 0.61538, 0.61538, 0.69231, 0.76923, 0.76923,
      0.76923, 0.84615, 0.92308, 0.92308, 0.92308, 0.92308, 0.92308,
      0.92308, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
    ),
    tolerance = 1e-3
  )
})


test_that("performance_roc, as.numeric", {
  data(iris)
  set.seed(123)
  iris$y <- rbinom(nrow(iris), size = 1, 0.3)
  folds <- sample(nrow(iris), size = nrow(iris) / 8, replace = FALSE)
  test_data <- iris[folds, ]
  train_data <- iris[-folds, ]

  model <- glm(y ~ Sepal.Length + Sepal.Width, data = train_data, family = "binomial")
  roc <- performance_roc(model)
  out <- as.numeric(roc)
  expect_equal(
    out,
    bayestestR::area_under_curve(roc$Specificity, roc$Sensitivity),
    tolerance = 1e-4,
    ignore_attr = TRUE
  )
})


test_that("performance_roc collapses tied predictions (row-order invariant)", {
  y <- c(1, 0, 1, 0, 1, 0)
  p <- c(0.5, 0.5, 0.5, 0.5, 0.2, 0.8)

  roc_fwd <- performance_roc(y, predictions = p)
  roc_rev <- performance_roc(rev(y), predictions = rev(p))

  expect_equal(roc_fwd$Sensitivity, roc_rev$Sensitivity, tolerance = 1e-8)
  expect_equal(roc_fwd$Specificity, roc_rev$Specificity, tolerance = 1e-8)

  auc_fwd <- bayestestR::area_under_curve(roc_fwd$Specificity, roc_fwd$Sensitivity)
  auc_rev <- bayestestR::area_under_curve(roc_rev$Specificity, roc_rev$Sensitivity)
  expect_equal(auc_fwd, auc_rev, tolerance = 1e-8)
  expect_equal(auc_fwd, 2 / 9, tolerance = 1e-8)

  # Discrete predictor model with many tied predictions
  d1 <- mtcars
  d2 <- mtcars[rev(seq_len(nrow(mtcars))), ]
  m1 <- glm(vs ~ cyl, data = d1, family = "binomial")
  m2 <- glm(vs ~ cyl, data = d2, family = "binomial")
  expect_equal(
    as.numeric(performance_roc(m1)),
    as.numeric(performance_roc(m2)),
    tolerance = 1e-8
  )

  preds <- stats::predict(m1, type = "response")
  n_pos <- sum(d1$vs == 1)
  n_neg <- sum(d1$vs == 0)
  expected_auc <- (sum(rank(preds)[d1$vs == 1]) - n_pos * (n_pos + 1) / 2) /
    (n_pos * n_neg)
  expect_equal(as.numeric(performance_roc(m1)), expected_auc, tolerance = 1e-8)
})
