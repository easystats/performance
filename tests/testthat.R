library(testthat)
library(performance)

if (length(strsplit(packageDescription("performance")$Version, "\\.")[[1]]) > 3) {
  Sys.setenv("RunAllinsightTests" = "yes")
} else {
  Sys.setenv("RunAllinsightTests" = "no")
}


test_check("performance")
