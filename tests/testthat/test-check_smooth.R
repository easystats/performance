if (requiet("testthat") && requiet("performance") && requiet("mgcv") && requiet("brms")) {
  test_that("check_smooth | mgcv", {
    skip_if_not_installed("mgcv")
    set.seed(333)

    m1 <- mgcv::gam(Sepal.Length ~ s(Petal.Length, k = 3) + s(Petal.Width, k=5), data = iris)
    m2 <- mgcv::gam(Sepal.Length ~ t2(Petal.Length, Petal.Width, k=5), data = iris)
    m3 <- mgcv::gam(Sepal.Length ~ s(Petal.Length, k = 3, by = Species), data = iris)

    rez1 <- check_smooth(m1, iterations=1000)
    rez2 <- check_smooth(m2, iterations=1000)
    rez3 <- check_smooth(m3, iterations=1000)

    c1 <- as.data.frame(mgcv::k.check(m1, n.rep=1000))
    c2 <- as.data.frame(mgcv::k.check(m2, n.rep=1000))
    c3 <- as.data.frame(mgcv::k.check(m3, n.rep=1000))

    # Deterministic
    expect_equal(max(rez1$EDF_max - c1$`k'`), 0, tolerance = 0)
    expect_equal(max(rez1$EDF - c1$edf), 0, tolerance = 0)
    expect_equal(max(rez2$EDF_max - c2$`k'`), 0, tolerance = 0)
    expect_equal(max(rez2$EDF - c2$edf), 0, tolerance = 0)
    expect_equal(max(rez3$EDF_max - c3$`k'`), 0, tolerance = 0)
    expect_equal(max(rez3$EDF - c3$edf), 0, tolerance = 0)

    # Random
    expect_equal(max(rez1$k - c1$`k-index`), 0, tolerance = 0.1)
    expect_equal(max(rez1$p - c1$`p-value`), 0, tolerance = 0.1)
    expect_equal(max(rez2$k - c2$`k-index`), 0, tolerance = 0.1)
    expect_equal(max(rez2$p - c2$`p-value`), 0, tolerance = 0.1)
    expect_equal(max(rez3$k - c3$`k-index`), 0, tolerance = 0.1)
    expect_equal(max(rez3$p - c3$`p-value`), 0, tolerance = 0.1)
  })

  test_that("check_smooth | brms", {
    skip_if_not_installed("brms")

    # m1 <- brms::brm(Sepal.Length ~ s(Petal.Length, k = 3) + s(Petal.Width, k=5), data = iris,
    #                 refresh = 0, iter = 200, algorithm = "meanfield")

    # brms models currently not supported
  })
}
