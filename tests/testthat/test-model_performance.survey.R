.runThisTest <- Sys.getenv("RunAllperformanceTests") == "yes"

if (.runThisTest && Sys.getenv("USER") != "travis") {
  if (require("testthat") && require("performance") && require("survey")) {
    test_that("model_performance.survey", {
      set.seed(123)
      data(api)
      dstrat <- survey::svydesign(
        id = ~1,
        strata = ~stype,
        weights = ~pw,
        data = apistrat,
        fpc = ~fpc
      )

      # model
      model <- survey::svyglm(
        formula = sch.wide ~ ell + meals + mobility,
        design = dstrat,
        family = quasibinomial()
      )

      mp <- suppressWarnings(model_performance(model))

      expect_equal(mp$R2, 0.02943044, tolerance = 0.01)
      expect_equal(mp$AIC, 183.9723, tolerance = 0.01)
      expect_equal(colnames(mp), c("AIC", "R2", "R2_adjusted", "RMSE", "SCORE_LOG", "PCP"))
    })


    test_that("model_performance.survey-cox", {
      data(pbc)
      pbc$randomized <- with(pbc, !is.na(trt) & trt > 0)
      biasmodel <- glm(randomized ~ age * edema, data = pbc, family = binomial)
      pbc$randprob <- fitted(biasmodel)
      if (is.null(pbc$albumin)) pbc$albumin <- pbc$alb ##pre2.9.0

      dpbc <- svydesign(
        id =  ~ 1,
        prob =  ~ randprob,
        strata =  ~ edema,
        data = subset(pbc, randomized)
      )
      rpbc <- as.svrepdesign(dpbc)
      model <- svycoxph(Surv(time, status > 0) ~ log(bili) + protime + albumin, design = dpbc)

      mp <- suppressWarnings(model_performance(model))

      expect_equal(mp$R2, 0.02943044, tolerance = 0.01)
      expect_equal(mp$AIC, 183.9723, tolerance = 0.01)
      expect_equal(colnames(mp), c("AIC", "R2", "R2_adjusted", "RMSE", "SCORE_LOG", "PCP"))
    })
  }
}