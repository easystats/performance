base <- iris
names(base) <- c("y1", "y2", "x1", "x2", "species")

test_that("fixest: r2", {
  skip_if_not_installed("fixest")
  library(fixest)
  res <- feols(y1 ~ x1 + x2 + x2^2 | species, base)
  r2_res <- performance::r2(res)

  expect_equal(r2_res$R2, fitstat(res, "r2")[[1]])
  expect_equal(r2_res$R2_adjusted, fitstat(res, "ar2")[[1]])
  expect_equal(r2_res$R2_within, fitstat(res, "wr2")[[1]])
  expect_equal(r2_res$R2_within_adjusted, fitstat(res, "war2")[[1]])
})


test_that("fixest: overdispersion", {
  skip_if_not_installed("fixest")
  library(fixest)
  res <- feols(y1 ~ x1 + x2 + x2^2 | species, base)
  expect_error(
    check_overdispersion(res),
    "can only be used for models from Poisson"
  )
})

test_that("fixest: outliers", {
  skip_if_not_installed("fixest")
  library(fixest)
  res <- feols(y1 ~ x1 + x2 + x2^2 | species, base)
  outliers_list <- suppressMessages(check_outliers(res))
  expect_identical(attr(outliers_list, "outlier_count"), list())
})

test_that("fixest: model_performance", {
  skip_if_not_installed("fixest")
  library(fixest)
  res <- feols(y1 ~ x1 + x2 + x2^2 | species, base)
  perf <- model_performance(res)
  expect_equal(perf$AIC, 107.743, tolerance = 1e-3)
  expect_equal(perf$BIC, 125.807, tolerance = 1e-3)
  expect_equal(perf$R2, 0.837, tolerance = 1e-3)
  expect_equal(perf$R2_adjusted, 0.832, tolerance = 1e-3)
  expect_equal(perf$R2_within, 0.573, tolerance = 1e-3)
  expect_equal(perf$R2_within_adjusted, 0.564, tolerance = 1e-3)
  expect_equal(perf$RMSE, 0.333, tolerance = 1e-3)
  expect_equal(perf$Sigma, 0.340, tolerance = 1e-3)
})



test_that("fixest_multi: r2", {
  skip_if_not_installed("fixest")
  library(fixest)
  res <- feols(c(y1, y2) ~ x1 + csw(x2, x2^2) | species, base)
  r2_res <- performance::r2(res)

  expect_equal(unname(r2_res[[1]]$R2), 0.837, tolerance = 1e-3)
})

test_that("fixest_multi: overdispersion", {
  skip_if_not_installed("fixest")
  library(fixest)
  res <- feols(c(y1, y2) ~ x1 + csw(x2, x2^2) | species, base)
  expect_error(
    check_overdispersion(res),
    "can only be used for models from Poisson"
  )
})

test_that("fixest_multi: outliers", {
  skip_if_not_installed("fixest")
  library(fixest)
  res <- feols(c(y1, y2) ~ x1 + csw(x2, x2^2) | species, base)
  outliers_list <- suppressMessages(check_outliers(res)[[1]])
  expect_identical(attr(outliers_list, "outlier_count"), list())
})

test_that("fixest_multi: model_performance", {
  skip_if_not_installed("fixest")
  library(fixest)
  res <- feols(c(y1, y2) ~ x1 + csw(x2, x2^2) | species, base)
  res2 <- feols(y1 ~ x1 + x2 + x2^2 | species, base)
  perf <- model_performance(res)
  perf2 <- model_performance(res2)
  expect_identical(perf[[2]], perf2)
})
