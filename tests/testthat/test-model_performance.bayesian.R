.runThisTest <- Sys.getenv("RunAllperformanceTests") == "yes"

if (.runThisTest &&
  require("testthat") &&
  require("performance") &&
  suppressPackageStartupMessages(require("rstanarm", quietly = TRUE)) &&
  suppressPackageStartupMessages(require("brms", quietly = TRUE))) {
  test_that("model_performance.stanreg", {
    set.seed(333)
    model <- insight::download_model("stanreg_lm_1")
    perf <- model_performance(model)

    expect_equal(perf$R2, 0.7398733, tolerance = 1e-3)
    expect_equal(perf$R2_adjusted, 0.7075685, tolerance = 1e-3)
    expect_equal(perf$ELPD, -83.49838, tolerance = 1e-3)

    model <- insight::download_model("stanreg_lm_2")
    perf <- model_performance(model)

    expect_equal(perf$R2, 0.8168386, tolerance = 1e-3)
    expect_equal(perf$R2_adjusted, 0.791236, tolerance = 1e-3)
    expect_equal(perf$ELPD, -78.38735, tolerance = 1e-3)

    model <- insight::download_model("stanreg_lmerMod_1")
    perf <- model_performance(model)

    expect_equal(perf$R2, 0.6286546, tolerance = 1e-3)
    expect_equal(perf$R2_adjusted, 0.5896637, tolerance = 1e-3)
    expect_equal(perf$ELPD, -31.55849, tolerance = 1e-3)
  })


  test_that("model_performance.brmsfit", {
    set.seed(333)

    model <- insight::download_model("brms_1")
    perf <- model_performance(model)
    expect_equal(perf$R2, 0.8262673, tolerance = 1e-3)
    expect_equal(perf$R2_adjusted, 0.7901985, tolerance = 1e-3)
    expect_equal(perf$ELPD, -78.59823, tolerance = 1e-3)

    model <- insight::download_model("brms_mixed_4")
    perf <- model_performance(model)
    expect_equal(perf$R2, 0.954538, tolerance = 1e-3)
    expect_equal(perf$R2_adjusted, 0.9523081, tolerance = 1e-3)
    expect_equal(perf$ELPD, -70.40493, tolerance = 1e-3)

    model <- insight::download_model("brms_ordinal_1")
    perf <- suppressWarnings(model_performance(model))
    expect_equal(perf$R2, 0.8760015, tolerance = 1e-3)
    expect_equal(perf$ELPD, -11.65433, tolerance = 1e-3)
  })
}

if (require("testthat") &&
  require("performance") &&
  suppressPackageStartupMessages(require("BayesFactor", quietly = TRUE)) &&
  suppressPackageStartupMessages(require("rstantools"))) {
  test_that("model_performance.BFBayesFactor", {
    mod <- ttestBF(mtcars$wt, mu = 3)
    expect_warning(p <- model_performance(mod))
    expect_null(p)

    mod <- ttestBF(mtcars$wt, factor(mtcars$am))
    expect_warning(p <- model_performance(mod))
    expect_null(p)

    mods <- contingencyTableBF(matrix(1:4, 2), sampleType = "indepMulti", fixedMargin = "cols")
    expect_warning(p <- model_performance(mod))
    expect_null(p)

    mod <- correlationBF(mtcars$wt, mtcars$am)
    expect_warning(p <- model_performance(mod))
    expect_null(p)

    mod <- proportionBF(y = 15, N = 25, p = .5)
    expect_warning(p <- model_performance(mod))
    expect_null(p)

    t <- c(-.15, 2.39, 2.42, 2.43, -.15, 2.39, 2.42, 2.43)
    N <- c(100, 150, 97, 99, 99, 97, 100, 150)
    mod <- meta.ttestBF(t, N)
    expect_warning(p <- model_performance(mod))
    expect_null(p)


    mod <- regressionBF(mpg ~ cyl, mtcars, progress = FALSE)
    modF <- lm(mpg ~ cyl, mtcars)
    p <- model_performance(mod)
    expect_equal(p$R2, unname(r2(modF)[[1]]), tolerance = 0.05)
    expect_equal(p$Sigma, sigma(modF), tolerance = 0.05)
  })
}
