if (require("testthat", quietly = TRUE)) {
  library(performance)

  is_dev_version <- length(strsplit(packageDescription("performance")$Version, "\\.")[[1]]) > 3L
  if (is_dev_version) {
    Sys.setenv("RunAllperformanceTests" = "yes")
  } else {
    Sys.setenv("RunAllperformanceTests" = "no")
  }

  test_check("performance")
}
