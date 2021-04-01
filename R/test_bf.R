#' @rdname test_performance
#' @export
test_bf <- function(...) {
  UseMethod("test_bf")
}


#' @importFrom insight ellipsis_info
#' @export
test_bf.default <- function(...) {

  # Attribute class to list and get names from the global environment
  objects <- insight::ellipsis_info(..., only_models = TRUE)
  names(objects) <- match.call(expand.dots = FALSE)$`...`

  # Sanity checks (will throw error if non-valid objects)
  .test_performance_checks(objects, multiple = FALSE)

  if (length(objects) == 1 && isTRUE(insight::is_model(objects))) {
    stop("'test_bf()' is designed to compare multiple models together. For a single model, you might want to run bayestestR::bf_parameters() instead.", call. = FALSE)
  }

  # If a suitable class is found, run the more specific method on it
  if (inherits(objects, c("ListNestedRegressions", "ListNonNestedRegressions", "ListLavaan"))) {
    test_bf(objects)
  } else {
    stop("The models cannot be compared for some reason :/")
  }
}



#' @importFrom bayestestR bayesfactor_models
#' @export
test_bf.ListModels <- function(objects, reference = 1, ...) {
  if (.test_bf_areAllBayesian(objects) == "mixed") {
    stop("You cannot mix Bayesian and non-Bayesian models in 'test_bf()'.", call. = FALSE)
  }

  # Adapt reference but keep original input
  if (reference == "sequential") {
    ref <- 1
  } else {
    ref <- reference
  }

  rez <- bayestestR::bayesfactor_models(objects, denominator = ref)
  row.names(rez) <- NULL

  # Adjust BFs for sequential testing
  if (reference == "sequential") {
    # TODO: Double check that formula and whether it works for increasing and
    # decreasing order.

    # For increasing
    rez$BF <- exp(c(NA, diff(log(rez$BF))))
    # For decreasing
    # rez$BF <- exp(c(-diff(log(rez$BF)), NA))
  }

  rez$BF[ref] <- NA

  # Replace denominator
  attr(rez, "denominator") <- ref
  class(rez) <- c("bayesfactor_models", "see_bayesfactor_models", class(rez))
  rez
}



# Helpers -----------------------------------------------------------------

#' @importFrom insight model_info
.test_bf_areAllBayesian <- function(objects) {
  bayesian_models <- sapply(objects, function(i) isTRUE(insight::model_info(i)$is_bayesian))
  if (all(bayesian_models == TRUE)) {
    "yes"
  } else if (all(bayesian_models == FALSE)) {
    "no"
  } else {
    "mixed"
  }
}
