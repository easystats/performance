test_that("check_outliers, Cook's distance with data methods that flag no row", {
  m <- lm(mpg ~ disp + drat, data = mtcars)
  cook_only <- as.data.frame(check_outliers(m, method = "cook"))
  expect_false(any(check_outliers(m, method = "zscore_robust")))
  out <- as.data.frame(check_outliers(m, method = c("cook", "zscore_robust")))
  expect_equal(out$Distance_Cook, cook_only$Distance_Cook, tolerance = 1e-10)
  expect_equal(out$Outlier, cook_only$Outlier_Cook / 2, tolerance = 1e-10)
})


test_that("check_outliers, Cook's distance for binomial models with only factor predictors", {
  skip_if_not_installed("lme4")
  data(cbpp, package = "lme4")
  m <- glm(
    cbind(incidence, size - incidence) ~ period,
    family = binomial,
    data = cbpp
  )
  out <- as.data.frame(check_outliers(m, method = "cook"))
  expect_equal(out$Distance_Cook, unname(stats::cooks.distance(m)), tolerance = 1e-8)
  expect_error(
    check_outliers(m, method = c("cook", "zscore")),
    regex = "No numeric variables found"
  )
})


test_that("check_outliers, default Cook's distance threshold uses p and n of the model", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("glmmTMB")
  data(sleepstudy, package = "lme4")
  d <- mtcars
  d$wt2 <- 2 * d$wt
  cook_threshold <- function(m) attr(check_outliers(m, method = "cook"), "threshold")$cook

  expect_equal(
    cook_threshold(lm(mpg ~ factor(cyl) + wt, data = mtcars)),
    stats::qf(0.5, 4, 28)
  )
  expect_equal(cook_threshold(lm(mpg ~ wt + wt2, data = d)), stats::qf(0.5, 2, 30))
  m <- glmmTMB::glmmTMB(Reaction ~ poly(Days, 2) + (1 | Subject), data = sleepstudy)
  expect_equal(cook_threshold(m), stats::qf(0.5, 3, 177))
})


test_that("check_outliers, Cook's distance for a saturated model gives an error", {
  m <- lm(mpg ~ wt + hp, data = mtcars[1:3, ])
  expect_error(
    check_outliers(m, method = "cook"),
    regex = "n\\s+=\\s+3\\s+observations\\s+and\\s+p\\s+=\\s+3\\s+fixed"
  )
})


test_that("check_outliers, glmmTMB Cook's distance refits use the data given to the model", {
  skip_if_not_installed("glmmTMB")
  d <- mtcars
  d$mpg[c(2, 9)] <- NA
  m <- glmmTMB::glmmTMB(mpg ~ scale(wt) + hp, data = d, subset = -1, REML = TRUE)
  expect_equal(
    as.data.frame(check_outliers(m, method = "cook"))$Distance_Cook,
    unname(stats::cooks.distance(lm(mpg ~ scale(wt) + hp, data = d, subset = -1))),
    tolerance = 1e-6
  )
})


test_that("check_outliers, glmmTMB Cook's distance errors if the data changed after the fit", {
  skip_on_cran()
  skip_if_not_installed("lme4")
  skip_if_not_installed("glmmTMB")
  data(sleepstudy, package = "lme4")

  d1 <- sleepstudy
  m1 <- glmmTMB::glmmTMB(Reaction ~ log(Days + 1) + (1 | Subject), data = d1)
  d1$Days <- rev(d1$Days)
  expect_error(
    check_outliers(m1, method = "cook"),
    regex = "because\\s+a\\s+refit\\s+on\\s+this\\s+data\\s+gives\\s+different"
  )

  d2 <- sleepstudy
  m2 <- glmmTMB::glmmTMB(Reaction ~ Days + (1 | Subject), data = d2)
  d2$Reaction <- rev(d2$Reaction)
  expect_error(
    check_outliers(m2, method = "cook"),
    regex = "fitted\\s+on\\.\\s+It\\s+may\\s+have\\s+changed"
  )
})


test_that("check_outliers, glmmTMB Cook's distance for a model fitted without data", {
  skip_if_not_installed("glmmTMB")
  set.seed(2)
  x <- rnorm(30)
  y <- x + rnorm(30)
  m <- suppressWarnings(glmmTMB::glmmTMB(y ~ x))
  out <- as.data.frame(check_outliers(m, method = "cook"))
  expect_identical(nrow(out), 30L)
  expect_true(all(is.finite(out$Distance_Cook)))
})


test_that("check_outliers, glmmTMB refits rejected with start values are tried without", {
  skip_if_not_installed("glmmTMB")
  m <- glmmTMB::glmmTMB(mpg ~ wt + hp, data = mtcars)
  update_model <- .update_model
  local_mocked_bindings(.update_model = function(x, data, start, weights, subset = NULL) {
    if (!is.null(start)) {
      stop("forced refit error", call. = FALSE)
    }
    update_model(x, data = data, start = start, weights = weights, subset = subset)
  })
  out <- as.data.frame(check_outliers(m, method = "cook"))
  expect_true(all(is.finite(out$Distance_Cook)))
})
