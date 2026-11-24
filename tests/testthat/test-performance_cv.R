test_that("performance_cv", {
  data(mtcars)
  model <- lm(mpg ~ wt + cyl, data = mtcars)

  set.seed(123)
  out <- performance_cv(model)
  expect_equal(out$MSE, 5.91153, tolerance = 1e-3)
  expect_named(out, c("MSE", "RMSE", "R2"))

  set.seed(123)
  out <- performance_cv(model, metrics = c("MSE", "R2"))
  expect_equal(out$MSE, 5.91153, tolerance = 1e-3)
  expect_named(out, c("MSE", "R2"))

  set.seed(123)
  out <- performance_cv(model, method = "loo")
  expect_equal(out$MSE, 7.376451, tolerance = 1e-3)

  set.seed(123)
  out <- performance_cv(model, method = "k_fold")
  expect_equal(out$MSE, 9.65578, tolerance = 1e-3)
})

test_that("performance_cv, loo shortcut matches refitting for least squares", {
  set.seed(3)
  d <- data.frame(x = rnorm(30))
  d$y <- 0.5 * d$x + rnorm(30)
  loo_refit <- function(model) {
    mean(vapply(
      seq_len(nrow(d)),
      function(i) {
        m_i <- stats::update(model, data = d[-i, ])
        (d$y[i] - stats::predict(m_i, newdata = d[i, ]))^2
      },
      numeric(1)
    ))
  }

  m_lm <- lm(y ~ x, data = d)
  expect_true(performance:::.loo_hat_shortcut_is_exact(m_lm))
  expect_equal(
    performance_cv(m_lm, method = "loo")$MSE,
    loo_refit(m_lm),
    tolerance = 1e-8
  )

  m_glm <- glm(y ~ x, data = d)
  expect_true(performance:::.loo_hat_shortcut_is_exact(m_glm))
  expect_equal(
    performance_cv(m_glm, method = "loo")$MSE,
    loo_refit(m_glm),
    tolerance = 1e-8
  )
})

test_that("performance_cv, loo refits mixed models instead of using hat values", {
  skip_if_not_installed("lme4")
  # hat values of a mixed model hold the variance parameters fixed at values
  # estimated with the held-out observation included, so e / (1 - h) is
  # optimistic; exact LOO requires refitting
  set.seed(3)
  g <- rep(1:6, each = 5)
  d <- data.frame(x = rnorm(30), g = factor(g))
  d$y <- 0.5 * d$x + rnorm(6)[g] * 0.7 + rnorm(30)
  m <- lme4::lmer(y ~ x + (1 | g), data = d)

  loo_refit <- mean(vapply(
    seq_len(nrow(d)),
    function(i) {
      m_i <- stats::update(m, data = d[-i, ])
      (d$y[i] - stats::predict(m_i, newdata = d[i, ]))^2
    },
    numeric(1)
  ))
  out <- performance_cv(m, method = "loo", verbose = FALSE)
  expect_equal(out$MSE, loo_refit, tolerance = 1e-6)
})
