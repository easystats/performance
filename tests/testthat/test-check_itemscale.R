if (requiet("parameters")) {
  # data generation
  set.seed(123)
  d <- data.frame(
    a = sample.int(3, 100, replace = TRUE) + 3,
    b = sample.int(3, 100, replace = TRUE) + 3,
    c = sample.int(3, 100, replace = TRUE) + 3,
    d = sample.int(3, 100, replace = TRUE),
    e = sample.int(3, 100, replace = TRUE),
    f = sample.int(3, 100, replace = TRUE)
  )

  test_that("check_convergence", {
    pca <- principal_components(d, n = 2)
    out <- check_itemscale(pca)
    expect_length(out, 2L)
    expect_equal(out[[1]]$Mean, vapply(d[out[[1]]$Item], mean, numeric(1)), tolerance = 1e-4, ignore_attr = TRUE)
    expect_equal(
      out[[2]]$Difficulty,
      item_difficulty(d[out[[2]]$Item])$Difficulty,
      tolerance = 1e-4,
      ignore_attr = TRUE
    )
  })
}
