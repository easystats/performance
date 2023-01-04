.runThisTest <- Sys.getenv("RunAllperformanceTests") == "yes"

if (.runThisTest && requiet("parameters") && requiet("fixest") && getRversion() >= "3.6.0") {
  data("qol_cancer", package = "parameters")
  qol_cancer <- cbind(
    qol_cancer,
    datawizard::demean(qol_cancer, select = c("phq4", "QoL"), group = "ID")
  )

  m <- feols(
    QoL ~ time + phq4 | ID,
    data = qol_cancer
  )
  mp <- model_performance(m)

  test_that("model_performance.fixest", {
  })
}
