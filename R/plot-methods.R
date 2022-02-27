#' @export
plot.performance_roc <- function(x, ...) {
  insight::check_if_installed("see", "to plot ROC-curves")
  NextMethod()
}


#' @export
plot.check_outliers <- function(x, ...) {
  insight::check_if_installed("see", "to plot outliers")
  NextMethod()
}



#' @export
plot.check_collinearity <- function(x, ...) {
  insight::check_if_installed("see", "to plot collinearity-check")
  NextMethod()
}



#' @export
plot.check_distribution <- function(x, ...) {
  insight::check_if_installed("see", "to plot predicted distributions")
  NextMethod()
}



#' @export
plot.check_distribution_numeric <- function(x, ...) {
  insight::check_if_installed("see", "to plot predicted distributions")
  NextMethod()
}



#' @export
plot.check_normality <- function(x, ...) {
  insight::check_if_installed("see", "for residual plots")
  NextMethod()
}


#' @export
plot.check_heteroscedasticity <- function(x, ...) {
  insight::check_if_installed("see", "for residual plots")
  NextMethod()
}


#' @export
plot.check_homogeneity <- function(x, ...) {
  insight::check_if_installed("see", "for homogeneity plots")
  NextMethod()
}


#' @export
plot.compare_performance <- function(x, ...) {
  insight::check_if_installed("see", "for model comparison plots")
  NextMethod()
}


#' @export
plot.check_model <- function(x, ...) {
  insight::check_if_installed("see", "for model diagnositic plots")
  NextMethod()
}

#' @export
plot.test_likelihoodratio <- function(x, ...) {
  warning(insight::format_message("There is currently no plot() method for test-functions.",
                                  "Please use 'plot(compare_perfomance())' for some visual representations of your model comparisons."), call. = FALSE)
}

#' @export
plot.test_performance <- function(x, ...) {
  warning(insight::format_message("There is currently no plot() method for test-functions.",
                                  "Please use 'plot(compare_perfomance())' for some visual representations of your model comparisons."), call. = FALSE)
}
