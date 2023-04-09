#' @rdname test_performance
#' @export
test_bf <- function(...) {
  UseMethod("test_bf")
}


#' @rdname test_performance
#' @export
test_bf.default <- function(..., reference = 1, text_length = NULL) {
  # Attribute class to list and get names from the global environment
  objects <- insight::ellipsis_info(..., only_models = TRUE)
  names(objects) <- match.call(expand.dots = FALSE)$`...`

  # Sanity checks (will throw error if non-valid objects)
  .test_performance_checks(objects, multiple = FALSE)

  if (length(objects) == 1 && isTRUE(insight::is_model(objects))) {
    insight::format_error(
      "`test_bf()` is designed to compare multiple models together. For a single model, you might want to run `bayestestR::bf_parameters()` instead."
    )
  }

  # If a suitable class is found, run the more specific method on it
  if (inherits(objects, c("ListNestedRegressions", "ListNonNestedRegressions", "ListLavaan"))) {
    test_bf(objects, reference = reference, text_length = text_length)
  } else {
    stop("The models cannot be compared for some reason :/", call. = FALSE)
  }
}



#' @export
test_bf.ListModels <- function(objects, reference = 1, text_length = NULL, ...) {
  if (.test_bf_areAllBayesian(objects) == "mixed") {
    insight::format_error("You cannot mix Bayesian and non-Bayesian models in `test_bf()`.")
  }

  # Adapt reference but keep original input
  if (reference == "sequential") {
    ref <- 1
  } else {
    ref <- reference
  }

  rez <- bayestestR::bayesfactor_models(objects, denominator = ref)

  # check for log-BF
  if (!is.null(rez$log_BF)) {
    rez$BF <- exp(rez$log_BF)
  }

  row.names(rez) <- NULL

  # Adjust BFs for sequential testing
  if (reference == "sequential") {
    # TODO: Double check that formula and whether it works for increasing and
    # decreasing order.

    # For increasing
    rez$BF <- exp(c(NA, diff(log(rez$BF))))

    # For decreasing
    # ref <- nrow(rez)
    # rez$BF <- exp(c(-diff(log(rez$BF)), NA))
  } else {
    rez$BF[ref] <- NA
  }

  # add log-BF
  rez$log_BF <- log(rez$BF)

  # Replace denominator
  attr(rez, "denominator") <- ref
  attr(rez, "text_length") <- text_length
  class(rez) <- c("bayesfactor_models", "see_bayesfactor_models", class(rez))
  rez
}



# Helpers -----------------------------------------------------------------

.test_bf_areAllBayesian <- function(objects) {
  bayesian_models <- sapply(objects, function(i) isTRUE(insight::model_info(i)$is_bayesian))

  if (all(bayesian_models)) {
    "yes"
  } else if (!all(bayesian_models)) {
    "no"
  } else {
    "mixed"
  }
}
