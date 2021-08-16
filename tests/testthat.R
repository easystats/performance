if (require("testthat")) {
  library(performance)

  if (length(strsplit(packageDescription("performance")$Version, "\\.")[[1]]) > 3) {
    Sys.setenv("RunAllperformanceTests" = "yes")
  } else {
    Sys.setenv("RunAllperformanceTests" = "no")
  }

  si <- Sys.info()

  osx <- tryCatch(
    {
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

  solaris <- tryCatch(
    {
      if (!is.null(si["sysname"])) {
        grepl("SunOS", si["sysname"], ignore.case = TRUE)
      } else {
        FALSE
      }
    },
    error = function(e) {
      FALSE
    }
  )

  if (!osx && !solaris) {
    test_check("performance")
  }
}
