test_that("test_vuong - nested", {
  skip_if_not_installed("nonnest2")

  m1 <- lm(Sepal.Length ~ Petal.Width * Species, data = iris)
  m2 <- lm(Sepal.Length ~ Petal.Width + Species, data = iris)
  m3 <- lm(Sepal.Length ~ Petal.Width, data = iris)

  rez <- test_vuong(m1, m2, m3)

  ref <- nonnest2::vuongtest(m1, m2, nested = TRUE)
  expect_equal(rez[2L, "Omega2"], ref$omega)
  expect_equal(rez[2L, "p_Omega2"], ref$p_omega)
  expect_equal(rez[2L, "LR"], ref$LRTstat)
  expect_equal(rez[2L, "p_LR"], ref$p_LRT$A)

  ref <- nonnest2::vuongtest(m2, m3, nested = TRUE)
  expect_equal(rez[3L, "Omega2"], ref$omega)
  expect_equal(rez[3L, "p_Omega2"], ref$p_omega)
  expect_equal(rez[3L, "LR"], ref$LRTstat)
  expect_equal(rez[3L, "p_LR"], ref$p_LRT$A)
})

# test_that("test_vuong - nested (reversed)", {
#   skip_if_not_installed("nonnest2")
#   m1 <- lm(mpg ~ wt + cyl, data = mtcars)
#   m2 <- lm(mpg ~ wt + cyl + gear, data = mtcars)
#   m3 <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
# })

test_that("test_vuong - non-nested", {
  skip_if_not_installed("nonnest2")

  m1 <- lm(Sepal.Length ~ Petal.Width, data = iris)
  m2 <- lm(Sepal.Length ~ Petal.Length, data = iris)
  m3 <- lm(Sepal.Length ~ Species, data = iris)

  rez <- test_vuong(m1, m2, m3)

  ref <- nonnest2::vuongtest(m1, m2)
  expect_equal(rez[2L, "Omega2"], ref$omega)
  expect_equal(rez[2L, "p_Omega2"], ref$p_omega)
  expect_equal(rez[2L, "LR"], ref$LRTstat)
  expect_equal(rez[2L, "p_LR"], ref$p_LRT$B)

  ref <- nonnest2::vuongtest(m1, m3)
  expect_equal(rez[3L, "Omega2"], ref$omega)
  expect_equal(rez[3L, "p_Omega2"], ref$p_omega)
  expect_equal(rez[3L, "LR"], ref$LRTstat)
  expect_equal(rez[3L, "p_LR"], ref$p_LRT$A)
})

test_that("test_vuong - non-nested respects reference != 1", {
  skip_if_not_installed("CompQuadForm")

  m1 <- lm(Sepal.Length ~ Petal.Width, data = iris)
  m2 <- lm(Sepal.Length ~ Petal.Length, data = iris)
  m3 <- lm(Sepal.Length ~ Species, data = iris)

  rez <- test_vuong(m1, m2, m3, reference = 2)

  # Reference row (row 2) should be NA
  expect_true(all(is.na(rez[2L, c("Omega2", "p_Omega2", "LR", "p_LR")])))

  # Row 1 should compare m2 (ref) vs m1, matching test_vuong(m2, m1) row 2
  pair_21 <- test_vuong(m2, m1)
  expect_equal(rez[1L, "Omega2"], pair_21[2L, "Omega2"], tolerance = 1e-6)
  expect_equal(rez[1L, "LR"], pair_21[2L, "LR"], tolerance = 1e-6)
  expect_equal(rez[1L, "p_LR"], pair_21[2L, "p_LR"], tolerance = 1e-6)

  # Row 3 should compare m2 (ref) vs m3, matching test_vuong(m2, m3) row 2
  pair_23 <- test_vuong(m2, m3)
  expect_equal(rez[3L, "Omega2"], pair_23[2L, "Omega2"], tolerance = 1e-6)
  expect_equal(rez[3L, "LR"], pair_23[2L, "LR"], tolerance = 1e-6)
  expect_equal(rez[3L, "p_LR"], pair_23[2L, "p_LR"], tolerance = 1e-6)
})

test_that("test_vuong - nested compares residual df numerically", {
  skip_if_not_installed("CompQuadForm")

  # Residual df = 10 vs 9: lexicographic sort puts "10" < "9", numeric sort puts 9 < 10
  d <- mtcars[1:12, ]
  m_small <- lm(mpg ~ wt, data = d)
  m_full <- lm(mpg ~ wt + hp, data = d)

  rez_asc <- test_vuong(m_small, m_full)
  rez_desc <- test_vuong(m_full, m_small)

  expect_equal(rez_asc[2L, "LR"], rez_desc[2L, "LR"], tolerance = 1e-6)
  expect_equal(rez_asc[2L, "p_LR"], rez_desc[2L, "p_LR"], tolerance = 1e-6)
  expect_gt(rez_asc[2L, "LR"], 0)
})
