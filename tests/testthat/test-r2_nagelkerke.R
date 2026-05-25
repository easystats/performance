test_that("r2_nagelkerke", {
  model <- glm(vs ~ wt + mpg, data = mtcars, family = "binomial")
  expect_equal(
    r2_nagelkerke(model),
    c(`Nagelkerke's R2` = 0.589959301837163),
    tolerance = 1e-3
  )
  expect_equal(
    r2(model),
    list(R2_Tjur = c(`Tjur's R2` = 0.477692621360749)),
    tolerance = 1e-3,
    ignore_attr = TRUE
  )
})

skip_if_not_installed("withr")

test_that("r2_nagelkerke", {
  skip_if_not_installed("MASS")
  withr::with_options(
    list(contrasts = c("contr.treatment", "contr.poly")),
    {
      data(housing, package = "MASS")
      model <- MASS::polr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)
      expect_equal(
        r2_nagelkerke(model),
        c(`Nagelkerke's R2` = 0.1084083),
        tolerance = 1e-3
      )
    }
  )
})

test_that("r2_nagelkerke, multinom, correct base-model with NA", {
  skip_on_cran()
  skip_if_not_installed("nnet")

  n_obs <- 1000
  softmax <- function(x) {
    exp(x - max(x)) / sum(exp(x - max(x)))
  }
  sample_y <- function(x) {
    sample(1:3, size = 1, prob = softmax(c(0.25 * x, -0.1 * x, 0 * x)))
  }
  set.seed(123)
  sim_df <- data.frame(x = rnorm(n_obs, 0, 1), y = NA)

  for (i in 1:nrow(sim_df)) {
    sim_df$y[i] <- sample_y(sim_df$x[i])
  }

  sim_df$x[1:500] <- NA
  sim_df2 <- sim_df[!is.na(sim_df$x), ]

  m1 <- nnet::multinom(y ~ x, data = sim_df, trace = FALSE)
  m2 <- nnet::multinom(y ~ x, data = sim_df2, trace = FALSE)

  out1 <- r2_nagelkerke(m1)
  out2 <- r2_nagelkerke(m2)
  expect_equal(out1, out2, tolerance = 1e-4, ignore_attr = TRUE)

  out1 <- r2_mcfadden(m1)
  out2 <- r2_mcfadden(m2)
  expect_equal(out1$R2, out2$R2, tolerance = 1e-4, ignore_attr = TRUE)
})
