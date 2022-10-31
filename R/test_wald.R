#' @rdname test_performance
#' @export
test_wald <- function(...) {
  UseMethod("test_wald")
}


#' @export
test_wald.default <- function(...) {
  # Attribute class to list and get names from the global environment
  objects <- insight::ellipsis_info(..., only_models = TRUE)

  # Sanity checks (will throw error if non-valid objects)
  .test_performance_checks(objects)

  # ensure proper object names
  objects <- .check_objectnames(objects, sapply(match.call(expand.dots = FALSE)$`...`, as.character))

  # If a suitable class is found, run the more specific method on it
  if (inherits(objects, c("ListNestedRegressions", "ListNonNestedRegressions", "ListLavaan"))) {
    test_wald(objects)
  } else {
    insight::format_error("The models cannot be compared for some reason :/")
  }
}



#' @export
test_wald.ListNestedRegressions <- function(objects, ...) {
  # for binomial models, only chisq-test
  if (all(attributes(objects)$is_binomial)) {
    insight::format_warning(
      "Using Wald's F-Test is inappropriate for models with `binomial` family.",
      "Running Likelihood Ratio Test (LRT) now."
    )
    return(test_likelihoodratio(objects))
  } else {
    out <- .test_wald(objects, test = "F")
  }

  attr(out, "is_nested") <- TRUE
  class(out) <- c("test_performance", class(out))
  out
}


#' @export
test_wald.ListNonNestedRegressions <- function(objects, ...) {
  insight::format_error("Wald tests cannot be run on non-nested models. Try `test_vuong()`.")
}

# Helpers --------------------------


.test_wald <- function(objects, test = "F") {
  # Compute stuff
  dfs <- sapply(objects, insight::get_df, type = "residual")

  # sort by df
  if (!all(sort(dfs) == dfs) && !all(sort(dfs) == rev(dfs))) {
    objects <- objects[order(dfs)]
    dfs <- dfs[order(dfs)]
  }

  dfs_diff <- c(NA, diff(sapply(objects, insight::get_df, type = "model")))
  dev <- as.numeric(lapply(objects, insight::get_deviance))
  dev_diff <- c(NA, -diff(dev))

  out <- data.frame(
    df = dfs,
    df_diff = dfs_diff,
    stringsAsFactors = FALSE
  )


  # Find reference-model related stuff
  refmodel <- order(dfs)[1]
  scale <- dev[refmodel] / dfs[refmodel]

  # test = "F"
  if (test == "F") {
    f_value <- (dev_diff / dfs_diff) / scale
    f_value[!is.na(f_value) & f_value < 0] <- NA # rather than p = 0
    out$`F` <- f_value
    p <- stats::pf(f_value, abs(dfs_diff), dfs[refmodel], lower.tail = FALSE)

    # test = "LRT"
  } else {
    chi2 <- dev_diff / scale * sign(dfs_diff)
    chi2[!is.na(chi2) & chi2 < 0] <- NA # rather than p = 0
    out$Chi2 <- chi2
    p <- stats::pchisq(chi2, abs(dfs_diff), lower.tail = FALSE)
  }
  out$p <- p

  row.names(out) <- NULL
  out <- cbind(.test_performance_init(objects), out)
  out
}
