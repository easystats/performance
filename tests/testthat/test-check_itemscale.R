test_that("check_itemscale", {
  skip_if_not_installed("parameters")

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
})
