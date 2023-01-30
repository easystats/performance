requiet("bigutilsr")
requiet("ICS")
requiet("dbscan")

test_that("zscore negative threshold", {
  expect_error(
    check_outliers(mtcars$mpg, method = "zscore", threshold = -1),
    "The `threshold` argument"
  )
})

test_that("lof illegal threshold", {
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
  expect_identical(
    which(check_outliers(mtcars, method = "mahalanobis_robust", threshold = 25)),
    as.integer(c(7, 9, 21, 24, 27, 28, 29, 31))
  )
})

## FIXME: Fails on CRAN/windows
# (should be fixed but not clear why method mcd needs a seed;
# there should not be an element of randomness to it I think)
test_that("mcd which", {
  set.seed(42)
  expect_identical(
    which(check_outliers(mtcars, method = "mcd", threshold = 45)),
    as.integer(c(7, 9, 19, 21, 24, 27, 28, 30, 31))
  )
})

## FIXME: Fails on CRAN/windows
# (current CRAN version rstan is not compatible with R > 4.2)
test_that("ics which", {
  testthat::skip_if_not(packageVersion("rstan") >= "2.26.0")
  set.seed(42)
  expect_identical(
    which(check_outliers(mtcars, method = "ics", threshold = 0.001)),
    as.integer(c(9, 29))
  )
})

test_that("optics which", {
  expect_identical(
    which(check_outliers(mtcars, method = "optics", threshold = 14)),
    as.integer(c(5, 7, 15, 16, 17, 24, 25, 29, 31))
  )
})

test_that("lof which", {
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
  expect_identical(
    which(check_outliers(mtcars, method = c("zscore", "iqr"))),
    31L
  )
})

# We exclude method ics because it is too slow
test_that("all methods which", {
  expect_identical(
    which(check_outliers(mtcars,
      method = c(
        "zscore", "zscore_robust", "iqr", "ci", "eti", "hdi", "bci",
        "mahalanobis", "mahalanobis_robust", "mcd", "optics", "lof"
      ),
      threshold = list(
        "zscore" = 2.2, "zscore_robust" = 2.2, "iqr" = 1.2,
        "ci" = 0.95, "eti" = 0.95, "hdi" = 0.90, "bci" = 0.95,
        "mahalanobis" = 20, "mahalanobis_robust" = 25, "mcd" = 25,
        "optics" = 14, "lof" = 0.005
      )
    )),
    as.integer(c(9, 15, 16, 19, 20, 28, 29, 31))
  )
})

# 5. Next, we test adding ID


test_that("multiple methods with ID", {
  data <- datawizard::rownames_as_column(mtcars, var = "car")
  x <- attributes(check_outliers(data,
    method = c(
      "zscore", "zscore_robust", "iqr", "ci", "eti", "hdi", "bci",
      "mahalanobis", "mahalanobis_robust", "mcd", "optics", "lof"
    ),
    threshold = list(
      "zscore" = 2.2, "zscore_robust" = 2.2, "iqr" = 1.2,
      "ci" = 0.95, "eti" = 0.95, "hdi" = 0.90, "bci" = 0.95,
      "mahalanobis" = 20, "mahalanobis_robust" = 25, "mcd" = 25,
      "optics" = 14, "lof" = 0.005
    ),
    ID = "car"
  ))
  expect_identical(
    x$outlier_var$zscore$mpg$car,
    "Toyota Corolla"
  )
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
  model <- lm(disp ~ mpg + hp, data = mtcars)
  expect_identical(
    which(check_outliers(model, method = c("cook", "optics", "lof"))),
    31L
  )
})

if (requiet("rstanarm")) {
  test_that("pareto which", {
    set.seed(123)
    model <- rstanarm::stan_glm(mpg ~ qsec + wt, data = mtcars)
    invisible(capture.output(model))

    expect_identical(
      which(check_outliers(model, method = "pareto", threshold = list(pareto = 0.5))),
      17L
    )
  })

  test_that("pareto multiple methods which", {
    set.seed(123)
    model <- rstanarm::stan_glm(mpg ~ qsec + wt, data = mtcars)
    invisible(capture.output(model))
    expect_identical(
      which(check_outliers(
        model,
        method = c("pareto", "optics"),
        threshold = list(pareto = 0.3, optics = 11)
      )),
      9L
    )
  })
}

if (requiet("BayesFactor")) {
  test_that("BayesFactor which", {
    set.seed(123)
    model <- BayesFactor::regressionBF(rating ~ ., data = attitude, progress = FALSE)
    expect_identical(
      which(check_outliers(model, threshold = list(mahalanobis = 15))),
      18L
    )
  })
}

# 7. Next, we test grouped output

test_that("cook multiple methods which", {
  iris2 <- datawizard::data_group(iris, "Species")
  z <- attributes(check_outliers(iris2, method = c("zscore", "iqr")))
  expect_named(
    z$outlier_count,
    c("setosa", "versicolor", "virginica")
  )
})
