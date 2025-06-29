test_that("check_itemscale", {
  skip_if_not_installed("parameters")
  skip_if_not_installed("psych")
  skip_if_not_installed("GPArotation")

  set.seed(123)
  d <- data.frame(
    a = sample.int(3, 100, replace = TRUE) + 3,
    b = sample.int(3, 100, replace = TRUE) + 3,
    c = sample.int(3, 100, replace = TRUE) + 3,
    d = sample.int(3, 100, replace = TRUE),
    e = sample.int(3, 100, replace = TRUE),
    f = sample.int(3, 100, replace = TRUE)
  )
  pca <- parameters::principal_components(d, n = 2)
  out <- check_itemscale(pca)
  expect_length(out, 2L)
  expect_equal(
    out[[1]]$Mean,
    vapply(d[out[[1]]$Item], mean, numeric(1)),
    tolerance = 1e-4,
    ignore_attr = TRUE
  )
  expect_equal(
    out[[2]]$Difficulty,
    item_difficulty(d[out[[2]]$Item])$Difficulty,
    tolerance = 1e-4,
    ignore_attr = TRUE
  )
  expect_snapshot(print(out), variant = "windows")
  comp <- parameters::closest_component(pca)
  out2 <- check_itemscale(d, comp)
  expect_equal(
    out[[1]]$Mean,
    out2[[1]]$Mean,
    tolerance = 1e-4,
    ignore_attr = TRUE
  )
  expect_equal(
    out[[1]]$Difficulty,
    out2[[1]]$Difficulty,
    tolerance = 1e-4,
    ignore_attr = TRUE
  )

  # factor_index as none-named vector
  out3 <- check_itemscale(d, factor_index = c(2, 1, 2, 2, 1, 1))
  expect_equal(
    out[[1]]$Mean,
    out3[[1]]$Mean,
    tolerance = 1e-4,
    ignore_attr = TRUE
  )
  expect_equal(
    out[[1]]$Difficulty,
    out3[[1]]$Difficulty,
    tolerance = 1e-4,
    ignore_attr = TRUE
  )
  expect_error(
    check_itemscale(d),
    regex = "If `x` is a data"
  )
  expect_error(
    check_itemscale(d, factor_index = 1:8),
    regex = "`factor_index` must be of same"
  )
  expect_error(
    check_itemscale(d, factor_index = factor(comp)),
    regex = "`factor_index` must be numeric."
  )
  expect_error(
    check_itemscale(iris$Species),
    regex = "`x` must be an object of class"
  )
})


test_that("check_itemscale for FA", {
  skip_if_not_installed("parameters")
  skip_if_not_installed("psych")
  skip_if_not_installed("GPArotation")

  f <- parameters::factor_analysis(mtcars, n = 2, rotation = "oblimin", standardize = FALSE)
  out <- check_itemscale(f)
  expect_equal(
    out[[1]]$Mean,
    c(20.09062, 6.1875, 146.6875, 17.84875, 0.4375, 2.8125),
    tolerance = 1e-4
  )
})


test_that("print_md check_itemscale for FA", {
  skip_if_not_installed("parameters")
  skip_if_not_installed("psych")
  skip_if_not_installed("GPArotation")

  f <- parameters::factor_analysis(mtcars, n = 2, rotation = "oblimin", standardize = FALSE)
  out <- check_itemscale(f)
  expect_snapshot(print_md(out))
})
