library(testthat)
library(performance)

if (length(strsplit(packageDescription("performance")$Version, "\\.")[[1]]) > 3) {
  Sys.setenv("RunAllperformanceTests" = "yes")
} else {
  Sys.setenv("RunAllperformanceTests" = "no")
}

osx <- tryCatch(
  {
    si <- Sys.info()
    if (!is.null(si["sysname"])) {
      si["sysname"] == "Darwin" || grepl("^darwin", R.version$os)
    } else {
      FALSE
    }
  },
  error = function(e) {
    FALSE
  }
)

if (!osx) {
  test_check("performance")
}
