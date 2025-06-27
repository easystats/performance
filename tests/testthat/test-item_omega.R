test_that("item_omega", {
  skip_if_not_installed("psych")
  skip_if_not_installed("parameters")
  skip_if_not_installed("GPArotation")

  data(mtcars)
  omega <- item_omega(mtcars, n = 3)

  expect_snapshot(print(omega))
  expect_snapshot(print_md(omega))

  expect_equal(
    as.numeric(omega),
    c(
      Alpha = 0.88219,
      G.6 = 0.97254,
      `Omega (hierarchical)` = 0.56637,
      `Omega (asymptotic H)` = 0.58222,
      `Omega (total)` = 0.97277
    ),
    tolerance = 1e-3
  )

  expect_snapshot(parameters::model_parameters(omega))
  expect_snapshot(summary(omega))
})


test_that("item_omega", {
  skip_on_cran()
  skip_if_not_installed("psych")
  skip_if_not_installed("parameters")
  skip_if_not_installed("GPArotation")
  skip_if_not_installed("correlation")
  skip_if_not_installed("discovr")

  # text matrix n_obs
  williams <- as.data.frame(discovr::williams)
  williams$ID <- NULL
  n <- 28
  r <- correlation::correlation(williams[1:n])

  # create r-matrix
  r_mat <- matrix(0, nrow = n, ncol = n)
  diag(r_mat) <- 1
  r_mat[lower.tri(r_mat)] <- r$r
  r_mat[upper.tri(r_mat)] <- r$r

  # create n-matrix
  n_mat <- matrix(0, nrow = n, ncol = n)
  diag(n_mat) <- 1
  n_mat[lower.tri(n_mat)] <- r$n_Obs
  n_mat[upper.tri(n_mat)] <- r$n_Obs

  out <- suppressWarnings(factor_analysis(r_mat, n_obs = n_mat, n = 2))
  expect_identical(dim(out), c(28L, 5L))
  expect_named(
    out,
    c("Variable", "MR1", "MR2", "Complexity", "Uniqueness")
  )

  n_mat <- matrix(0, nrow = n - 2, ncol = n - 2)
  diag(n_mat) <- 1
  n_mat[lower.tri(n_mat)] <- r$n_Obs[1:325]
  n_mat[upper.tri(n_mat)] <- r$n_Obs[1:325]

  # matrix dimensions do not match
  expect_error(
    suppressWarnings(factor_analysis(r_mat, n_obs = n_mat, n = 2)),
    regex = "The provided"
  )
})
