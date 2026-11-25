#' @export
format.compare_performance <- function(x, digits = 2, ...) {
  # if we have ranking, add score and remove incomplete indices in print
  if ("Performance_Score" %in% colnames(x)) {
    x$Performance_Score <- insight::format_value(x$Performance_Score, as_percent = TRUE)
    x <- x[!sapply(x, anyNA)]
  }

  # format weighted ICs
  weighted_ics <- endsWith(colnames(x), "_wt")
  if (any(weighted_ics)) {
    x[weighted_ics] <- lapply(x[weighted_ics], function(wt) {
      out <- insight::format_bf(wt, name = NULL, exact = TRUE)
      # a model without this IC has no weight, not a weight of 1
      out[is.na(wt)] <- NA
      out
    })
  }

  if ("BF" %in% colnames(x)) {
    x$BF[is.na(x$BF)] <- 1
    # x$BF <- insight::format_bf(x$BF, exact = TRUE)
  }
  x <- .format_lavaan_robust(x)
  out <- insight::format_table(x, digits = digits, ...)

  # drop the empty "()" that format_table() adds for a missing weight
  weight_columns <- endsWith(colnames(out), "(weights)")
  out[weight_columns] <- lapply(out[weight_columns], function(i) {
    trimws(gsub("()", "", i, fixed = TRUE))
  })
  out
}


#' @export
format.performance_model <- function(x, digits = 2, ...) {
  # format p-values for meta-analysis
  x <- .format_lavaan_robust(x)
  insight::format_table(x, digits = digits, ...)
}


# helper ------------------------------------------------------------------

# insight::format_table() formats "Chi2" with "Chi2_df" and the p-values of
# lavaan's standard test as "Chi2(df)" and "p (Chi2)". Do the same for the
# scaled test and the robust RMSEA of models with a robust estimator.
.format_lavaan_robust <- function(x) {
  if (all(c("Chi2_scaled", "Chi2_scaled_df") %in% colnames(x))) {
    dof <- unique(x$Chi2_scaled_df[!is.na(x$Chi2_scaled_df)])
    if (length(dof) == 1) {
      dof <- insight::format_value(dof, protect_integers = TRUE)
      colnames(x)[colnames(x) == "Chi2_scaled"] <- paste0("Chi2_scaled(", dof, ")")
      x$Chi2_scaled_df <- NULL
    }
  }
  for (p_column in c("p_Chi2_scaled", "p_RMSEA_robust")) {
    if (p_column %in% colnames(x)) {
      x[[p_column]] <- format(
        insight::format_p(x[[p_column]], name = NULL, missing = ""),
        justify = "left"
      )
      colnames(x)[colnames(x) == p_column] <- paste0(
        "p (",
        sub("^p_", "", p_column),
        ")"
      )
    }
  }
  x
}
