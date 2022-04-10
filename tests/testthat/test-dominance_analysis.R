if (requiet("testthat") && requiet("performance") && requiet("domir") &&
    (R.version$major > 3 | (R.version$major == 3 && R.version$minor >= 5))) {
  data(mtcars)
  DA_test_model <- lm(mpg ~ vs + cyl + carb, data = mtcars)
  DA_performance <- dominance_analysis(DA_test_model)
  DA_domir <- domin(mpg ~ vs + cyl + carb, lm, list(performance::r2, "R2"), data = mtcars)

  test_that("dominance_analysis$general_dominance", {
    expect_equal(DA_domir$General_Dominance,DA_performance$general_dominance)
  })

  test_that("dominance_analysis$conditional_dominance", {
    expect_equal(DA_domir$Conditional_Dominance,DA_performance$conditional_dominance)
  })

  DA_perf_cpt <- t(DA_performance$complete_dominance)
  dimnames(DA_perf_cpt) <-
    list(paste0("Dmnates_", rownames(DA_performance$complete_dominance)),
         colnames(DA_performance$complete_dominance))

  test_that("dominance_analysis$complete_dominance", {
    expect_equal(DA_domir$Complete_Dominance,DA_perf_cpt)
  })

  DA_performance2 <-
    dominance_analysis(DA_test_model, all = ~ vs,  sets = c(~carb),
                       complete = FALSE, conditional = FALSE)
  DA_domir2 <- domin(mpg ~ cyl, lm, list(performance::r2, "R2"),
                     all = "vs", sets = list("carb"), data = mtcars,
                     conditional = FALSE, complete = FALSE)

  test_that("dominance_analysis$general_dominance with sets/all", {
    expect_equal(DA_domir2$General_Dominance,DA_performance2$general_dominance)

    expect_equal(DA_domir2$Fit_Statistic_All_Subsets, DA_performance2$all_subset_R2)

    expect_equal(DA_domir2$Conditional_Dominance,DA_performance2$conditional_dominance)
  })


}
