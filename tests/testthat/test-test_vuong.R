requiet("nonnest2")
requiet("lavaan")

test_that("test_vuong - nested", {
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
# m1 <- lm(mpg ~ wt + cyl, data = mtcars)
# m2 <- lm(mpg ~ wt + cyl + gear, data = mtcars)
# m3 <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
# })


test_that("test_vuong - non-nested", {
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
