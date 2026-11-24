test_that("r2_mckelvey works for binomial cloglog link", {
  m_cloglog <- stats::glm(vs ~ wt + mpg, data = mtcars, family = stats::binomial("cloglog"))
  out <- r2_mckelvey(m_cloglog)

  yhat <- stats::predict(m_cloglog, type = "link")
  ss <- sum((yhat - mean(yhat))^2)
  expected <- ss / (nrow(mtcars) * (pi^2 / 6) + ss)

  expect_false(is.na(out))
  expect_equal(as.numeric(out), expected, tolerance = 1e-6)
  expect_named(out, "McKelvey's R2")
})
