test_that("r2 lm", {
  data(iris)
  model <- lm(Sepal.Length ~ Species + Petal.Length, data = iris)
  out <- r2(model)
  expect_equal(out$R2, c(R2 = 0.83672), tolerance = 1e-3)
  expect_equal(out$R2_adjusted, c(`adjusted R2` = 0.83337), tolerance = 1e-3)
})

test_that("r2 lm, ci", {
  data(iris)
  model <- lm(Sepal.Length ~ Species + Petal.Length, data = iris)
  out <- r2(model, ci = 0.95)
  expect_equal(
    out$R2,
    c(R2 = 0.83672, CI_low = 0.77725, CI_high = 0.87665),
    tolerance = 1e-3
  )
  expect_equal(
    out$R2_adjusted,
    c(`adjusted R2` = 0.83337, CI_low = 0.77282, CI_high = 0.87406),
    tolerance = 1e-3
  )
})

test_that("r2 glm", {
  data(mtcars)
  model <- glm(am ~ mpg, data = mtcars)
  out <- r2(model)
  expect_equal(out$R2, c(R2 = 0.3598), tolerance = 1e-3)
})

test_that("r2 glm, ci", {
  data(mtcars)
  model <- glm(am ~ mpg, data = mtcars)
  out <- r2(model, ci = 0.95)
  expect_equal(
    out$R2,
    c(R2 = 0.3598, CI_low = 0.09758, CI_high = 0.6066),
    tolerance = 1e-3
  )
})

# glmmTMB, non-mixed --------------------------------------------------------

skip_if_not_installed("withr")
withr::with_environment(
  new.env(),
  test_that("r2 glmmTMB, no ranef", {
    skip_if_not_installed("glmmTMB")
    data(Owls, package = "glmmTMB")
    # linear ---------------------------------------------------------------
    m <- glmmTMB::glmmTMB(NegPerChick ~ BroodSize + ArrivalTime, data = Owls)
    out <- r2(m)
    expect_equal(out$R2, 0.05597288, tolerance = 1e-3, ignore_attr = TRUE)
    # validate against lm
    m2 <- lm(NegPerChick ~ BroodSize + ArrivalTime, data = Owls)
    out2 <- r2(m2)
    expect_equal(out$R2, out2$R2, tolerance = 1e-3, ignore_attr = TRUE)
    # binomial -------------------------------------------------------------
    data(mtcars)
    m <- glmmTMB::glmmTMB(am ~ mpg, data = mtcars, family = binomial())
    out <- r2(m)
    expect_equal(out[[1]], 0.3677326, tolerance = 1e-3, ignore_attr = TRUE)
    # validate against glm
    m2 <- glm(am ~ mpg, data = mtcars, family = binomial())
    out2 <- r2(m2)
    expect_equal(out[[1]], out2[[1]], tolerance = 1e-3, ignore_attr = TRUE)
    # poisson --------------------------------------------------------------
    d <<- data.frame(
      counts = c(18, 17, 15, 20, 10, 20, 25, 13, 12),
      outcome = gl(3, 1, 9),
      treatment = gl(3, 3)
    )
    m <- glmmTMB::glmmTMB(counts ~ outcome + treatment, family = poisson(), data = d)
    out <- r2(m)
    expect_equal(out[[1]], 0.6571698, tolerance = 1e-3, ignore_attr = TRUE)
    # validate against glm
    m2 <- glm(counts ~ outcome + treatment, family = poisson(), data = d)
    out2 <- r2(m2)
    expect_equal(out[[1]], out2[[1]], tolerance = 1e-3, ignore_attr = TRUE)
    # zero-inflated --------------------------------------------------------------
    skip_if_not_installed("pscl")
    data(bioChemists, package = "pscl")
    m <- glmmTMB::glmmTMB(
      art ~ fem + mar + kid5 + ment,
      ziformula = ~ kid5 + phd,
      family = poisson(),
      data = bioChemists
    )
    out <- r2(m)
    expect_equal(out[[1]], 0.1797549, tolerance = 1e-3, ignore_attr = TRUE)
    # validate against pscl::zeroinfl
    m2 <- pscl::zeroinfl(
      art ~ fem + mar + kid5 + ment | kid5 + phd,
      data = bioChemists
    )
    out2 <- r2(m2)
    expect_equal(out[[1]], out2[[1]], tolerance = 1e-3, ignore_attr = TRUE)
    # Gamma --------------------------------------------------------------
    clotting <<- data.frame(
      u = c(5, 10, 15, 20, 30, 40, 60, 80, 100),
      lot1 = c(118, 58, 42, 35, 27, 25, 21, 19, 18),
      lot2 = c(69, 35, 26, 21, 18, 16, 13, 12, 12)
    )
    m <- suppressWarnings(glmmTMB::glmmTMB(lot1 ~ log(u), data = clotting, family = Gamma()))
    out <- r2(m)
    expect_equal(out[[1]], 0.996103, tolerance = 1e-3, ignore_attr = TRUE)
    # validate against glm
    m2 <- glm(lot1 ~ log(u), data = clotting, family = Gamma())
    out2 <- r2(m2)
    expect_equal(out[[1]], out2[[1]], tolerance = 1e-3, ignore_attr = TRUE)
  })
)
