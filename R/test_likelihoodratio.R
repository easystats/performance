#' @rdname test_performance
#' @param estimator Applied when comparing regression models using
#'   `test_likelihoodratio()`. Corresponds to the different estimators for
#'   the standard deviation of the errors. If `estimator="OLS"` (default),
#'   then it uses the same method as `anova(..., test="LRT")` implemented
#'   in base R, i.e., scaling by n-k (the unbiased OLS estimator) and using this
#'   estimator under the alternative hypothesis. If `estimator="ML"`, which
#'   is for instance used by `lrtest(...)` in package \pkg{lmtest}, the
#'   scaling is done by n (the biased ML estimator) and the estimator under the
#'   null hypothesis. In moderately large samples, the differences should be
#'   negligible, but it is possible that OLS would perform slightly better in
#'   small samples with Gaussian errors.
#' @export
test_likelihoodratio <- function(..., estimator = "ML") {
  UseMethod("test_likelihoodratio")
}


# Should we deprecate this name? And print a warning not to use this name anymore and that we will remove it in future versions?
#' @rdname test_performance
#' @export
performance_lrt <- test_likelihoodratio


# Short name for test_likelihoodratio using the test_* naming convention
#' @rdname test_performance
#' @export
test_lrt <- test_likelihoodratio


#' @export
test_likelihoodratio.default <- function(..., estimator = "ML") {

  # Attribute class to list
  objects <- insight::ellipsis_info(..., only_models = TRUE)

  # Sanity checks (will throw error if non-valid objects)
  .test_performance_checks(objects)

  # Replace with names from the global environment
  object_names <- match.call(expand.dots = FALSE)$`...`
  names(objects) <- object_names

  # If a suitable class is found, run the more specific method on it
  if (inherits(objects, "ListNestedRegressions")) {
    test_likelihoodratio(objects, estimator = estimator)
  } else if (inherits(objects, "ListLavaan")) {
    test_likelihoodratio_ListLavaan(..., objects = objects) # Because lavaanLRT requires the ellipsis
  } else {
    stop("The models are not nested models, which is a prerequisite for `test_likelihoodratio()`. See the 'Details' section.")
  }
}



#' @export
test_likelihoodratio.ListNestedRegressions <- function(objects, estimator = "ML", ...) {
  dfs <- sapply(objects, insight::get_df, type = "model")

  # sort by df
  if (!all(sort(dfs) == dfs) && !all(sort(dfs) == rev(dfs))) {
    objects <- objects[order(dfs)]
    dfs <- dfs[order(dfs)]
  }

  dfs_diff <- c(NA, diff(dfs))

  # lmtest::lrtest()
  if (tolower(estimator) %in% c("ml", "mle")) {
    lls <- sapply(objects, insight::get_loglikelihood)
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
  } else {
    out <- .test_wald(objects, test = "LRT")
    out$df <- dfs # Replace residual df with model's df
  }


  attr(out, "is_nested_increasing") <- attributes(objects)$is_nested_increasing
  attr(out, "is_nested_decreasing") <- attributes(objects)$is_nested_decreasing
  class(out) <- c("test_likelihoodratio", "see_test_likelihoodratio", "data.frame")
  out
}




test_likelihoodratio_ListLavaan <- function(..., objects = NULL) {
  insight::check_if_installed("lavaan")

  # Create data frame with info about model name and class
  names_types <- data.frame(Model = names(objects), Type = sapply(objects, function(x) class(x)[1]), stringsAsFactors = FALSE)

  out <- as.data.frame(lavaan::lavTestLRT(..., test = "LRT", model.names = names(objects)), stringsAsFactors = FALSE)

  # Rename columns
  colnames(out)[names(out) == "Df"] <- "df"
  colnames(out)[names(out) == "Df diff"] <- "df_diff"
  colnames(out)[names(out) == "Chisq"] <- "Chi2"
  colnames(out)[grepl("^Pr\\(>", names(out))] <- "p"
  out$Model <- row.names(out)

  # Bind all data
  out <- merge(names_types, out[c("Model", "df", "df_diff", "Chi2", "p")], by = "Model")

  class(out) <- c("test_likelihoodratio", "see_test_likelihoodratio", "data.frame")
  out
}
