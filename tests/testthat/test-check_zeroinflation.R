test_that("check_zeroinflation", {
  skip_if_not_installed("glmmTMB")
  set.seed(123)
  data(Salamanders, package = "glmmTMB")
  m <- glm(count ~ spp + mined, family = poisson, data = Salamanders)

  expect_equal(
    check_zeroinflation(m),
    structure(
      list(
        predicted.zeros = 298,
        observed.zeros = 387L,
        ratio = 0.770025839793282,
        tolerance = 0.05
      ),
      class = "check_zi"
    ),
    tolerance = 1e-3
  )
})


test_that("check_zeroinflation, glmmTMB with and without zero-inflation component", {
  skip_if_not_installed("glmmTMB")
  skip_if_not_installed("DHARMa")
  set.seed(123)
  data(Salamanders, package = "glmmTMB")

  # no zero-inflation model
  m <- glmmTMB::glmmTMB(count ~ spp + mined, family = poisson, data = Salamanders)

  expect_equal(
    check_zeroinflation(m),
    structure(
      list(
        predicted.zeros = 298,
        observed.zeros = 387L,
        ratio = 0.770025839793282,
        tolerance = 0.05
      ),
      class = "check_zi"
    ),
    tolerance = 1e-3
  )

  # zero-inflation model
  m <- glmmTMB::glmmTMB(
    count ~ spp + mined,
    ziformula = ~ spp + mined,
    family = poisson,
    data = Salamanders
  )

  set.seed(123)
  expect_equal(
    check_zeroinflation(m),
    structure(
      list(
        predicted.zeros = 387,
        observed.zeros = 387L,
        ratio = 1.00093023255814,
        tolerance = 0.1,
        p.value = 1
      ),
      class = "check_zi"
    ),
    tolerance = 1e-3
  )
})


test_that("check_zeroinflation, glmer.nb", {
  skip_on_cran()
  skip_if_not_installed("glmmTMB")
  skip_if_not_installed("lme4")
  set.seed(101)
  data(Salamanders, package = "glmmTMB")
  dd <- expand.grid(
    f1 = factor(1:3),
    f2 = LETTERS[1:2],
    g = 1:9,
    rep = 1:15,
    KEEP.OUT.ATTRS = FALSE
  )
  mu <- 5 * (-4 + with(dd, as.integer(f1) + 4 * as.numeric(f2)))
  dd$y <- rnbinom(nrow(dd), mu = mu, size = 0.5)
  dat2 <<- dd
  suppressMessages({
    m <- lme4::glmer.nb(y ~ f1 * f2 + (1 | g), data = dat2, verbose = FALSE)
  })

  expect_equal(
    check_zeroinflation(m),
    structure(
      list(
        predicted.zeros = 153, observed.zeros = 155L,
        ratio = 0.987096774193548, tolerance = 0.05
      ),
      class = "check_zi"
    ),
    tolerance = 1e-3
  )
})


test_that("check_zeroinflation, glmmTMB nbinom", {
  skip_if_not_installed("glmmTMB")
  skip_if_not_installed("DHARMa")
  skip_on_cran()

  set.seed(1234)
  dat <- DHARMa::createData(sampleSize = 1000)
  fit <- suppressWarnings(glmmTMB::glmmTMB(
    observedResponse ~ Environment1 + (1 | group),
    data = dat,
    family = glmmTMB::nbinom1()
  ))
  expect_equal(
    check_zeroinflation(fit),
    structure(
      list(
        predicted.zeros = 462,
        observed.zeros = 482L,
        ratio = 0.95850622406639,
        tolerance = 0.1,
        p.value = 0.776
      ),
      class = "check_zi"
    ),
    tolerance = 1e-3
  )
})
