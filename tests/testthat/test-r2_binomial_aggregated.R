test_that("r2 functions warn and return NULL for aggregated binomial models", {
  agg <- do.call(
    rbind,
    lapply(split(mtcars, mtcars$cyl), function(d) {
      data.frame(cyl = d$cyl[1], success = sum(d$am), fail = sum(1 - d$am))
    })
  )
  m <- glm(cbind(success, fail) ~ factor(cyl), data = agg, family = binomial())

  expect_warning(out <- r2_tjur(m), "not Bernoulli", fixed = TRUE)
  expect_null(out)
  expect_warning(out <- r2_efron(m), "not Bernoulli", fixed = TRUE)
  expect_null(out)
  expect_warning(out <- r2_mckelvey(m), "not Bernoulli", fixed = TRUE)
  expect_null(out)

  skip_if_not_installed("correlation")
  skip_if_not_installed("Hmisc")
  expect_warning(out <- r2_somers(m), "not Bernoulli", fixed = TRUE)
  expect_null(out)
})


test_that("r2 functions give values for Bernoulli models", {
  m <- glm(am ~ factor(cyl), data = mtcars, family = binomial())
  expect_equal(r2_efron(m), 0.2731479, tolerance = 1e-4)
  expect_equal(r2_mckelvey(m), c(`McKelvey's R2` = 0.3122), tolerance = 1e-3)
  skip_if_not_installed("correlation")
  skip_if_not_installed("Hmisc")
  expect_equal(r2_somers(m), c(`Somers' Dxy` = 0.5708502), tolerance = 1e-4)
})
