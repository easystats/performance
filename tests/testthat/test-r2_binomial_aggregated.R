# The same data as Bernoulli rows, as proportions with weights, and as a
# two-column (successes, failures) matrix response.
.aggregated_binomial_models <- function() {
  agg <- do.call(
    rbind,
    lapply(split(mtcars, mtcars$cyl), function(d) {
      data.frame(
        cyl = d$cyl[1],
        p = mean(d$am),
        trials = nrow(d),
        success = sum(d$am),
        fail = sum(1 - d$am)
      )
    })
  )
  list(
    bernoulli = glm(am ~ factor(cyl), data = mtcars, family = binomial()),
    proportion = glm(
      p ~ factor(cyl),
      data = agg,
      weights = trials,
      family = binomial()
    ),
    matrix = glm(cbind(success, fail) ~ factor(cyl), data = agg, family = binomial())
  )
}

test_that("r2_tjur warns and returns NULL for aggregated binomial models", {
  models <- .aggregated_binomial_models()
  expect_silent(out <- r2_tjur(models$bernoulli))
  expect_equal(out, c(`Tjur's R2` = 0.2731479), tolerance = 1e-4)
  for (m in models[c("proportion", "matrix")]) {
    expect_warning(out <- r2_tjur(m), "not Bernoulli", fixed = TRUE)
    expect_null(out)
  }
})

test_that("r2_efron warns and returns NULL for aggregated binomial models", {
  models <- .aggregated_binomial_models()
  expect_silent(out <- r2_efron(models$bernoulli))
  expect_equal(out, 0.2731479, tolerance = 1e-4)
  for (m in models[c("proportion", "matrix")]) {
    expect_warning(out <- r2_efron(m), "not Bernoulli", fixed = TRUE)
    expect_null(out)
  }
  # non-binomial models are not affected
  counts <- c(18, 17, 15, 20, 10, 20, 25, 13, 12)
  outcome <- gl(3, 1, 9)
  treatment <- gl(3, 3)
  m_pois <- glm(counts ~ outcome + treatment, family = poisson())
  expect_silent(out <- r2_efron(m_pois))
  expect_type(out, "double")
})

test_that("r2_mckelvey warns and returns NULL for aggregated binomial models", {
  models <- .aggregated_binomial_models()
  expect_silent(out <- r2_mckelvey(models$bernoulli))
  expect_equal(out, c(`McKelvey's R2` = 0.3122), tolerance = 1e-3)
  for (m in models[c("proportion", "matrix")]) {
    expect_warning(out <- r2_mckelvey(m), "not Bernoulli", fixed = TRUE)
    expect_null(out)
  }
})

test_that("r2_somers warns and returns NULL for aggregated binomial models", {
  skip_if_not_installed("correlation")
  skip_if_not_installed("Hmisc")
  models <- .aggregated_binomial_models()
  expect_silent(out <- r2_somers(models$bernoulli))
  expect_equal(out, c(`Somers' Dxy` = 0.5708502), tolerance = 1e-4)
  for (m in models[c("proportion", "matrix")]) {
    expect_warning(out <- r2_somers(m), "not Bernoulli", fixed = TRUE)
    expect_null(out)
  }
})
