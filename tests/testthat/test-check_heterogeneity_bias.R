test_that("check_heterogeneity_bias", {
  data(iris)
  set.seed(123)
  iris$ID <- sample.int(4, nrow(iris), replace = TRUE) # fake-ID
  out <- check_heterogeneity_bias(iris, select = c("Sepal.Length", "Petal.Length"), by = "ID")
  expect_equal(out, c("Sepal.Length", "Petal.Length"), ignore_attr = TRUE)
  expect_output(print(out), "Possible heterogeneity bias due to following predictors: Sepal\\.Length, Petal\\.Length")

  out <- check_heterogeneity_bias(iris, select = ~ Sepal.Length + Petal.Length, by = ~ID)
  expect_equal(out, c("Sepal.Length", "Petal.Length"), ignore_attr = TRUE)
  expect_output(print(out), "Possible heterogeneity bias due to following predictors: Sepal\\.Length, Petal\\.Length")

  m <- lm(Sepal.Length ~ Petal.Length + Petal.Width + Species + ID, data = iris)
  expect_error(
    check_heterogeneity_bias(m, select = c("Sepal.Length", "Petal.Length"), by = "ID"),
    regex = "no mixed model"
  )

  skip_if_not_installed("lme4")
  m <- lme4::lmer(Sepal.Length ~ Petal.Length + Petal.Width + Species + (1 | ID), data = iris)
  out <- check_heterogeneity_bias(m, select = c("Sepal.Length", "Petal.Length"), by = "ID")
  expect_equal(out, c("Petal.Length", "Petal.Width", "Species"), ignore_attr = TRUE)
  expect_output(
    print(out),
    "Possible heterogeneity bias due to following predictors: Petal\\.Length, Petal\\.Width, Species"
  )
  out <- check_heterogeneity_bias(m, select = ~ Sepal.Length + Petal.Length, by = ~ID)
  expect_equal(out, c("Petal.Length", "Petal.Width", "Species"), ignore_attr = TRUE)
  expect_output(
    print(out),
    "Possible heterogeneity bias due to following predictors: Petal\\.Length, Petal\\.Width, Species"
  )
})
