test_that("performance_roc", {
  skip_if_not_installed("lme4")
  m <- lme4::glmer(vs ~ mpg + (1 | gear), family = "binomial", data = mtcars)
  roc <- performance_roc(m)
  expect_equal(
    roc$Sensitivity,
    c(
      0, 0.07143, 0.14286, 0.21429, 0.28571, 0.35714, 0.42857, 0.5,
      0.57143, 0.57143, 0.64286, 0.64286, 0.64286, 0.71429, 0.78571,
      0.85714, 0.85714, 0.92857, 0.92857, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1
    ),
    tolerance = 1e-3
  )
})

test_that("performance_roc", {
  set.seed(123)
  d <- iris[sample(1:nrow(iris), size = 50), ]
  d$y <- as.factor(rbinom(nrow(d), size = 1, 0.3))
  dat <<- d

  m <- glm(
    y ~ Sepal.Length + Sepal.Width,
    data = dat,
    family = "binomial"
  )
  roc <- performance_roc(m)
  expect_equal(
    roc$Sensitivity,
    c(
      0, 0, 0.07692, 0.07692, 0.07692, 0.15385, 0.23077, 0.23077,
      0.23077, 0.23077, 0.23077, 0.30769, 0.30769, 0.30769, 0.30769,
      0.30769, 0.38462, 0.38462, 0.38462, 0.46154, 0.46154, 0.53846,
      0.53846, 0.53846, 0.53846, 0.61538, 0.61538, 0.61538, 0.61538,
      0.61538, 0.69231, 0.76923, 0.76923, 0.76923, 0.84615, 0.92308,
      0.92308, 0.92308, 0.92308, 0.92308, 0.92308, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1
    ),
    tolerance = 1e-3
  )
})
