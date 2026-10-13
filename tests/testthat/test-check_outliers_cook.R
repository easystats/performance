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


test_that("check_outliers, default Cook's distance threshold uses p and n of the model", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("glmmTMB")
  data(sleepstudy, package = "lme4")
  d <- mtcars
  d$wt2 <- 2 * d$wt

  m_factor <- lm(mpg ~ factor(cyl) + wt, data = mtcars)
  m_poly <- lm(mpg ~ poly(wt, 3), data = mtcars)
  m_lm <- lm(mpg ~ wt + hp, data = mtcars)
  m_aliased <- lm(mpg ~ wt + wt2, data = d)
  m_lmer <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
  m_glmmtmb <- glmmTMB::glmmTMB(Reaction ~ poly(Days, 2) + (1 | Subject), data = sleepstudy)

  # p and n computed independently of performance
  expected <- function(m, p) stats::qf(0.5, p, stats::nobs(m) - p)
  cook_threshold <- function(m, method = "cook") {
    attr(check_outliers(m, method = method), "threshold")$cook
  }

  for (m in list(m_factor, m_poly, m_lm, m_aliased)) {
    expect_equal(cook_threshold(m), expected(m, sum(!is.na(stats::coef(m)))), tolerance = 1e-12)
  }
  expect_equal(cook_threshold(m_lmer), expected(m_lmer, length(lme4::fixef(m_lmer))), tolerance = 1e-12)
  expect_equal(
    cook_threshold(m_glmmtmb),
    expected(m_glmmtmb, length(glmmTMB::fixef(m_glmmtmb)$cond)),
    tolerance = 1e-12
  )
  expect_equal(
    cook_threshold(m_poly, method = c("cook", "zscore_robust")),
    expected(m_poly, sum(!is.na(stats::coef(m_poly)))),
    tolerance = 1e-12
  )
  # unchanged for a model whose numeric data columns match its fixed effects
  expect_equal(cook_threshold(m_lm), stats::qf(0.5, 3, 29), tolerance = 1e-12)
})

