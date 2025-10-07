test_that("zscore negative threshold", {
  expect_error(
    check_outliers(mtcars$mpg, method = "zscore", threshold = -1),
    "The `threshold` argument"
  )
})

test_that("lof illegal threshold", {
  skip_if_not_installed("dbscan")
  expect_error(
    check_outliers(mtcars$mpg, method = "lof", threshold = -1),
    "The `threshold` argument"
  )
  expect_error(
    check_outliers(mtcars$mpg, method = "lof", threshold = 1.1),
    "The `threshold` argument"
  )
})

# 1. We first test that each method consistently flags outliers,
# (given a specific threshold)

test_that("zscore which", {
  expect_identical(
    which(check_outliers(mtcars$mpg, method = "zscore", threshold = 2.2)),
    20L
  )
})

test_that("zscore_robust which", {
  expect_identical(
    which(check_outliers(mtcars$mpg, method = "zscore_robust", threshold = 2.2)),
    as.integer(c(18, 20))
  )
})

test_that("iqr which", {
  expect_identical(
    which(check_outliers(mtcars$mpg, method = "iqr", threshold = 1.2)),
    as.integer(c(18, 20))
  )
})

test_that("ci which", {
  expect_identical(
    which(check_outliers(mtcars$mpg, method = "ci", threshold = 0.95)),
    20L
  )
})

test_that("eti which", {
  expect_identical(
    which(check_outliers(mtcars$mpg, method = "eti", threshold = 0.95)),
    20L
  )
})

test_that("hdi which", {
  expect_identical(
    which(check_outliers(mtcars$mpg, method = "hdi", threshold = 0.90)),
    as.integer(c(18, 20))
  )
})

test_that("bci which", {
  expect_identical(
    which(check_outliers(mtcars$mpg, method = "bci", threshold = 0.95)),
    as.integer(c(15, 16, 20))
  )
})

test_that("mahalanobis which", {
  expect_identical(
    which(check_outliers(mtcars, method = "mahalanobis", threshold = 20)),
    as.integer(c(9, 29))
  )
})

test_that("mahalanobis_robust which", {
  skip_if_not_installed("bigutilsr")
  expect_identical(
    which(check_outliers(mtcars, method = "mahalanobis_robust", threshold = 25)),
    as.integer(c(7, 9, 21, 24, 27, 28, 29, 31))
  )
})

test_that("mcd which", {
  skip_if_not_installed("MASS")
  # (not clear why method mcd needs a seed)
  set.seed(42)
  expect_identical(
    tail(which(check_outliers(mtcars[1:4], method = "mcd", threshold = 45, verbose = FALSE))),
    31L
  )
  expect_warning(
    {
      out <- check_outliers(mtcars, method = "mcd")
    },
    regex = "The sample size is too small"
  )
  expect_identical(sum(out), 8L)
  out <- check_outliers(mtcars, method = "mcd", percentage_central = 0.5, verbose = FALSE)
  expect_identical(sum(out), 15L)
})

## FIXME: Fails on CRAN/windows
test_that("ics which", {
  # suddenly fails on R Under development (unstable) (2023-09-07 r85102)
  # gcc-13 (Debian 13.2.0-2) 13.2.0
  skip_on_cran()
  skip_if_not_installed("ICS")
  skip_if_not_installed("ICSOutlier")
  expect_identical(
    which(check_outliers(mtcars, method = "ics", threshold = 0.001)),
    as.integer(c(9, 29))
  )
})

test_that("optics which", {
  skip_if_not_installed("dbscan")
  expect_identical(
    which(check_outliers(mtcars, method = "optics", threshold = 14)),
    as.integer(c(5, 7, 15, 16, 17, 24, 25, 29, 31))
  )
})

test_that("lof which", {
  skip_if_not_installed("dbscan")
  expect_identical(
    which(check_outliers(mtcars, method = "lof", threshold = 0.005)),
    31L
  )
})

# 2. Next, we check the print method

test_that("zscore print", {
  expect_output(
    print(check_outliers(mtcars, method = "zscore", threshold = 2.2)),
    "5 outliers detected: cases 9, 16, 19, 20, 31."
  )
})

# 3. Next, we check some attributes since it looks harder than
# expected to test the complex print output itself

test_that("attributes threshold", {
  x <- attributes(check_outliers(mtcars, method = "zscore", threshold = 2.2))
  expect_equal(
    as.numeric(x$threshold),
    2.2
  )
})

test_that("attributes method", {
  x <- attributes(check_outliers(mtcars, method = "zscore", threshold = 2.2))
  expect_identical(
    x$method,
    "zscore"
  )
})

test_that("attributes variables", {
  x <- attributes(check_outliers(mtcars, method = "zscore", threshold = 2.2))
  expect_identical(
    x$variables,
    c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb")
  )
})

test_that("attributes data", {
  x <- attributes(check_outliers(mtcars, method = "zscore", threshold = 2.2))
  expect_s3_class(x$data, "data.frame")
})

test_that("attributes raw data", {
  x <- attributes(check_outliers(mtcars, method = "zscore", threshold = 2.2))
  expect_s3_class(x$raw_data, "data.frame")
})

test_that("attributes univariate data frames", {
  x <- attributes(check_outliers(mtcars, method = "zscore", threshold = 2.2))
  expect_s3_class(x$outlier_var$zscore$mpg, "data.frame")
})

test_that("attributes outlier count data frame", {
  x <- attributes(check_outliers(mtcars, method = "zscore", threshold = 2.2))
  expect_s3_class(x$outlier_count$all, "data.frame")
})

# 4. Next, we test multiple simultaneous methods

test_that("multiple methods which", {
  data <- rbind(mtcars, 99)
  expect_identical(
    which(check_outliers(data, method = c("zscore", "iqr"))),
    33L
  )
})

# We exclude method ics because it is too slow
test_that("all methods which", {
  skip_if_not_installed("bigutilsr")
  skip_if_not_installed("MASS")
  skip_if_not_installed("dbscan")
  skip_if_not_installed("ICS")
  skip_if_not_installed("ICSOutlier")
  skip_if_not_installed("loo")

  expect_identical(
    which(check_outliers(mtcars,
      method = c(
        "zscore", "zscore_robust", "iqr", "ci", "eti", "hdi", "bci",
        "mahalanobis", "mahalanobis_robust", "mcd", "optics", "lof"
      ),
      threshold = list(
        zscore = 2.2, zscore_robust = 2.2, iqr = 1.2,
        ci = 0.95, eti = 0.95, hdi = 0.90, bci = 0.95,
        mahalanobis = 20, mahalanobis_robust = 25, mcd = 25,
        optics = 14, lof = 0.005
      ),
      verbose = FALSE
    )),
    as.integer(c(9, 15, 16, 19, 20, 28, 29, 31))
  )
})

# 5. Next, we test adding ID


test_that("multiple methods with ID", {
  skip_if_not_installed("bigutilsr")
  skip_if_not_installed("MASS")
  skip_if_not_installed("dbscan")
  skip_if_not_installed("ICS")
  skip_if_not_installed("ICSOutlier")
  skip_if_not_installed("loo")

  data <- datawizard::rownames_as_column(mtcars, var = "car")
  x <- attributes(check_outliers(data,
    method = c(
      "zscore", "zscore_robust", "iqr", "ci", "eti", "hdi", "bci",
      "mahalanobis", "mahalanobis_robust", "mcd", "optics", "lof"
    ),
    threshold = list(
      zscore = 2.2, zscore_robust = 2.2, iqr = 1.2,
      ci = 0.95, eti = 0.95, hdi = 0.90, bci = 0.95,
      mahalanobis = 20, mahalanobis_robust = 25, mcd = 25,
      optics = 14, lof = 0.005
    ),
    ID = "car",
    verbose = FALSE
  ))
  expect_identical(
    x$outlier_var$zscore$mpg$car,
    "Toyota Corolla"
  )
  skip_if(getRversion() > "4.4.0")
  expect_identical(
    x$outlier_count$all$car[1],
    "Maserati Bora"
  )
})


# 6. Next, we test models

test_that("cook which", {
  model <- lm(disp ~ mpg + hp, data = mtcars)
  expect_identical(
    which(check_outliers(model, method = "cook", threshold = list(cook = 0.85))),
    31L
  )
})

## FIXME: Cannot use a single threshold, but only for this case
# test_that("cook which", {
#   model <- lm(disp ~ mpg + hp, data = mtcars)
#   expect_equal(
#     which(check_outliers(model, method = "cook", threshold = 0.85)),
#     # Error: The `threshold` argument must be NULL (for default values) or a list containing
#       threshold values for desired methods (e.g., `list('mahalanobis' = 7)`).
#     31
#   )
# })

test_that("cook multiple methods which", {
  skip_if_not_installed("dbscan")
  model <- lm(disp ~ mpg + hp, data = mtcars)
  expect_identical(
    which(check_outliers(model, method = c("cook", "optics", "lof"))),
    31L
  )
})

test_that("pareto which", {
  skip_on_cran()
  skip_if_not_installed("dbscan")
  skip_if_not_installed("loo")
  skip_if_not_installed("rstanarm")
  set.seed(123)
  model <- rstanarm::stan_glm(mpg ~ qsec + wt, data = mtcars, refresh = 0)
  invisible(capture.output(model))

  expect_identical(
    which(check_outliers(model, method = "pareto", threshold = list(pareto = 0.5))),
    c(17L, 18L)
  )
})

test_that("pareto multiple methods which", {
  skip_if_not_installed("dbscan")
  skip_if_not_installed("loo")
  skip_if_not_installed("rstanarm")
  set.seed(123)
  model <- rstanarm::stan_glm(mpg ~ qsec + wt, data = mtcars, refresh = 0)
  invisible(capture.output(model))
  expect_identical(
    which(check_outliers(
      model,
      method = c("pareto", "optics"),
      threshold = list(pareto = 0.3, optics = 11)
    )),
    20L
  )
})


test_that("BayesFactor which", {
  skip_if_not_installed("BayesFactor")
  set.seed(123)
  model <- BayesFactor::regressionBF(rating ~ ., data = attitude, progress = FALSE)
  expect_identical(
    which(check_outliers(model, threshold = list(mahalanobis = 15))),
    18L
  )
})


# 7. Next, we test grouped output

test_that("cook multiple methods which", {
  iris2 <- datawizard::data_group(iris, "Species")
  z <- attributes(check_outliers(iris2, method = c("zscore", "iqr")))
  expect_named(
    z$outlier_count,
    c("setosa", "versicolor", "virginica")
  )
})


test_that("check_outliers with invald data", {
  dd <- data.frame(y = as.difftime(0:5, units = "days"))
  m1 <- lm(y ~ 1, data = dd)
  expect_error(
    expect_message(
      check_outliers(m1),
      regex = "Date variables are not supported"
    ),
    regex = "No numeric variables found"
  )
})


test_that("check_outliers on numeric data only", {
  data(mtcars)
  # all predictors categorical
  mtcars$wt <- as.factor(mtcars$wt)
  mtcars$mpg <- as.factor(mtcars$mpg)
  model <- glm(vs ~ wt + mpg, data = mtcars, family = "binomial")
  expect_error(check_outliers(model), regex = "No numeric")
})


test_that("check_outliers with DHARMa", {
  skip_if_not_installed("DHARMa")
  mt1 <- mtcars[, c(1, 3, 4)]
  # create some fake outliers and attach outliers to main df
  mt2 <- rbind(mt1, data.frame(
    mpg = c(37, 40), disp = c(300, 400),
    hp = c(110, 120)
  ))
  # fit model with outliers
  model <- lm(disp ~ mpg + hp, data = mt2)
  set.seed(123)
  res <- simulate_residuals(model)
  out <- check_outliers(res)
  expect_equal(
    out,
    structure(
      list(
        Coefficient = 0.0294117647058824, Expected = 0.00796812749003984,
        CI_low = 0.000744364234690261, CI_high = 0.153267669560318,
        p_value = 0.238146844116552
      ),
      class = c("check_outliers_simres", "list")
    ),
    ignore_attr = TRUE,
    tolerance = 1e-4
  )
  expect_identical(
    capture.output(print(out)),
    c(
      "# Outliers detection", "", "  Proportion of observed outliers: 2.94%",
      "  Proportion of expected outliers: 0.80%, 95% CI [0.07, 15.33]",
      ""
    )
  )
})


test_that("check_outliers numeric, z-score", {
  data(mtcars)
  out <- check_outliers(mtcars$mpg, method = "zscore", threshold = 2)
  expect_equal(which(as.numeric(out) == 1), c(18, 20))
})


test_that("check_outliers with psych", {
  skip_if_not_installed("psych")
  data(Harman.5, package = "psych")
  pc <- psych::principal(Harman.5, 2, rotate = "varimax")
  out <- check_outliers(pc)
  expect_equal(which(as.numeric(out) == 1), 5)
  out <- check_outliers(pc, threshold = 0.1)
  expect_equal(which(as.numeric(out) == 1), integer(0))
  out <- check_outliers(pc, threshold = 0.02)
  expect_equal(which(as.numeric(out) == 1), c(3, 5, 6, 8))
})
