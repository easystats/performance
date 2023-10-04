test_that("check_heterogeneity_bias", {
  data(iris)
  set.seed(123)
  iris$ID <- sample(1:4, nrow(iris), replace = TRUE) # fake-ID
  out <- check_heterogeneity_bias(iris, select = c("Sepal.Length", "Petal.Length"), group = "ID")
  expect_equal(out, c("Sepal.Length", "Petal.Length"), ignore_attr = TRUE)
  expect_output(print(out), "Possible heterogeneity bias due to following predictors: Sepal\\.Length, Petal\\.Length")
})
