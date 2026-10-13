test_that("check_outliers, Cook's distance with data methods that flag no row", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("glmmTMB")
  data(sleepstudy, package = "lme4")
  models <- list(
    lm = lm(mpg ~ disp + drat, data = mtcars),
    lmer = lme4::lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy),
    glmmTMB = glmmTMB::glmmTMB(Reaction ~ Days + (1 | Subject), data = sleepstudy)
  )
  for (m in models) {
    cook_only <- as.data.frame(check_outliers(m, method = "cook"))
    for (data_method in c("zscore_robust", "iqr")) {
      # the data method alone flags no row
      expect_false(any(check_outliers(m, method = data_method)))

      out <- as.data.frame(check_outliers(m, method = c("cook", data_method)))
      expect_equal(out$Distance_Cook, cook_only$Distance_Cook, tolerance = 1e-10)
      expect_equal(out$Outlier, cook_only$Outlier_Cook / 2, tolerance = 1e-10)
    }
    out <- as.data.frame(check_outliers(m, method = c("cook", "zscore_robust", "iqr")))
    expect_equal(out$Distance_Cook, cook_only$Distance_Cook, tolerance = 1e-10)
  }
})
