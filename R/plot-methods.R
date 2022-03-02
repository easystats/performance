#' @export
plot.performance_roc <- function(x, ...) {
  insight::check_if_installed("see", "to plot ROC-curves")
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
