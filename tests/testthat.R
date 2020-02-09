library(testthat)
library(performance)

if (length(strsplit(packageDescription("performance")$Version, "\\.")[[1]]) > 3) {
  Sys.setenv("RunAllperformanceTests" = "yes")
} else {
  Sys.setenv("RunAllperformanceTests" = "no")
}


test_check("performance")
