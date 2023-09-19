#' @rdname test_performance
#' @param estimator Applied when comparing regression models using
#'   `test_likelihoodratio()`. Corresponds to the different estimators for
#'   the standard deviation of the errors. Defaults to `"OLS"` for linear models,
#'   `"ML"` for all other models (including mixed models), or `"REML"` for
#'   linear mixed models when these have the same fixed effects. See 'Details'.
#' @export
test_likelihoodratio <- function(..., estimator = "ML", verbose = TRUE) {
  UseMethod("test_likelihoodratio")
}


# Short name for test_likelihoodratio using the test_* naming convention
#' @rdname test_performance
#' @export
test_lrt <- test_likelihoodratio



# default --------------------

#' @export
test_likelihoodratio.default <- function(..., estimator = "OLS", verbose = TRUE) {
  # Attribute class to list
  objects <- insight::ellipsis_info(..., only_models = TRUE)

  # Sanity checks (will throw error if non-valid objects)
  objects <- .test_performance_checks(objects, verbose = verbose)

  # different default when mixed model or glm is included
  if (missing(estimator)) {
    mixed_models <- sapply(objects, insight::is_mixed_model)
    if (all(mixed_models) && all(sapply(objects, .is_lmer_reml)) && isTRUE(attributes(objects)$same_fixef)) {
      estimator <- "REML"
    } else if (any(mixed_models) || !all(attributes(objects)$is_linear)) {
      estimator <- "ML"
    }
  }

  # ensure proper object names
  objects <- .check_objectnames(objects, sapply(match.call(expand.dots = FALSE)$`...`, as.character))

  # If a suitable class is found, run the more specific method on it
  if (inherits(objects, "ListNestedRegressions")) {
    test_likelihoodratio(objects, estimator = estimator)
  } else if (inherits(objects, "ListLavaan")) {
    test_likelihoodratio_ListLavaan(..., objects = objects) # Because lavaanLRT requires the ellipsis
  } else {
    insight::format_error(
      "The models are not nested, which is a prerequisite for `test_likelihoodratio()`.",
      "See the 'Details' section.",
      "You may try `test_vuong()` instead."
    )
  }
}



# methods ------------------------------

#' @export
plot.test_likelihoodratio <- function(x, ...) {
  insight::format_alert(
    "There is currently no `plot()` method for test-functions.",
    "Please use `plot(compare_perfomance())` for some visual representations of your model comparisons."
  )
}


#' @export
print.test_likelihoodratio <- function(x, digits = 2, ...) {
  # Footer
  if ("LogLik" %in% names(x)) {
    best <- which.max(x$LogLik)
    footer <- c(sprintf("\nModel '%s' seems to have the best model fit.\n", x$Model[best]), "yellow")
  } else {
    footer <- NULL
  }

  # value formatting
  x$p <- insight::format_p(x$p, name = NULL)

  if (is.null(attributes(x)$estimator)) {
    estimator_string <- ""
  } else {
    estimator_string <- sprintf(" (%s-estimator)", toupper(attributes(x)$estimator))
  }

  cat(insight::export_table(
    x,
    digits = digits,
    caption = c(paste0("# Likelihood-Ratio-Test (LRT) for Model Comparison", estimator_string), "blue"),
    footer = footer
  ))

  invisible(x)
}



# other classes ---------------------------

#' @export
test_likelihoodratio.ListNestedRegressions <- function(objects, estimator = "ML", verbose = TRUE, ...) {
  dfs <- sapply(objects, insight::get_df, type = "model")
  same_fixef <- attributes(objects)$same_fixef

  # sort by df
  if (!all(sort(dfs) == dfs) && !all(sort(dfs) == rev(dfs))) {
    objects <- objects[order(dfs)]
    dfs <- sort(dfs, na.last = TRUE)
  }

  dfs_diff <- c(NA, diff(dfs))
  REML <- tolower(estimator) == "reml"

  # default anova(test="LRT")
  if (tolower(estimator) == "ols") {
    out <- .test_wald(objects, test = "LRT")
    out$df <- dfs # Replace residual df with model's df
  } else {
    # lmtest::lrtest()
    lls <- sapply(objects, insight::get_loglikelihood, REML = REML, check_response = TRUE)
    chi2 <- abs(c(NA, -2 * diff(lls)))
    p <- stats::pchisq(chi2, abs(dfs_diff), lower.tail = FALSE)

    out <- data.frame(
      df = dfs,
      df_diff = dfs_diff,
      Chi2 = chi2,
      p = p,
      stringsAsFactors = FALSE
    )

    out <- cbind(.test_performance_init(objects), out)
  }

  # for REML fits, warn user
  if (isTRUE(REML) &&
    # only when mixed models are involved, others probably don't have problems with REML fit
    any(sapply(objects, insight::is_mixed_model)) &&
    # only if not all models have same fixed effects (else, REML is ok)
    !isTRUE(same_fixef) && isTRUE(verbose)) {
    insight::format_warning(
      "The Likelihood-Ratio-Test is probably inaccurate when comparing REML-fit models with different fixed effects."
    )
  }

  attr(out, "is_nested_increasing") <- attributes(objects)$is_nested_increasing
  attr(out, "is_nested_decreasing") <- attributes(objects)$is_nested_decreasing
  attr(out, "estimator") <- tolower(estimator)
  class(out) <- c("test_likelihoodratio", "see_test_likelihoodratio", "data.frame")
  out
}


test_likelihoodratio_ListLavaan <- function(..., objects = NULL) {
  insight::check_if_installed("lavaan")

  # Create data frame with info about model name and class
  names_types <- data.frame(
    Model = names(objects),
    Type = sapply(objects, function(x) class(x)[1]),
    stringsAsFactors = FALSE
  )

  out <- as.data.frame(
    lavaan::lavTestLRT(..., test = "LRT", model.names = names(objects)),
    stringsAsFactors = FALSE
  )

  # Rename columns
  colnames(out)[names(out) == "Df"] <- "df"
  colnames(out)[names(out) == "Df diff"] <- "df_diff"
  colnames(out)[names(out) == "Chisq"] <- "Chi2"
  colnames(out)[startsWith(names(out), "Pr(>")] <- "p"
  out$Model <- row.names(out)

  # Bind all data
  out <- merge(names_types, out[c("Model", "df", "df_diff", "Chi2", "p")], by = "Model")

  class(out) <- c("test_likelihoodratio", "see_test_likelihoodratio", "data.frame")
  out
}


# helper ----------------------

.is_lmer_reml <- function(x) {
  tryCatch(inherits(x, "lmerMod") && as.logical(x@devcomp$dims[["REML"]]),
    error = function(e) FALSE
  )
}
