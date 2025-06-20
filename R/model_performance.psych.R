#' Performance of FA / PCA models
#'
#' Compute indices of model performance for models from the **psych** package,
#' and for `parameters::factor_analysis()` and `item_omega()`.
#'
#' @param model A model object of class `fa` (e.g., from `psych::fa()`),
#' `principal` (e.g., from `psych::principal()`), or from
#' `parameters::factor_analysis()` or `item_omega()`.
#' @param metrics Can be `"all"` or a character vector of metrics to be computed
#' (some of `"Chi2"`, `"Chi2_df"`, `"df"`, `"p_Chi2"`, `"RMSA"`, `"RMSA_corrected"`,
#' `"TLI"`, `"RMSEA"`, `"RMSEA_CI_low"`, `"RMSEA_CI_high"`, and `"BIC"`. For
#' omega-models, can also include `"R2"` and `"Correlation"`.
#' @param verbose Toggle off warnings.
#' @param ... Arguments passed to or from other methods.
#'
#' @details
#' For omega-models, the columns `R2` and `Correlation` are measures of factor
#' score adequacy. `R2` refers to the multiple R square of scores with factors,
#' while `Correlation` indicates the correlation of scores with factors.
#'
#' @return A data frame (with one row) and one column per "index" (see
#' `metrics`).
#'
#' @examplesIf all(insight::check_if_installed(c("psych", "GPArotation", "psychTools"), quietly = TRUE))
#' out <- psych::fa(psychTools::bfi[, 1:25], 5)
#' model_performance(out)
#' @export
model_performance.fa <- function(model, metrics = "all", verbose = TRUE, ...) {
  out <- data.frame(
    Chi2 = ifelse(is.null(model$STATISTIC), NA_real_, model$STATISTIC),
    Chi2_df = ifelse(is.null(model$dof), NA_real_, model$dof),
    p_Chi2 = ifelse(is.null(model$PVAL), NA_real_, model$PVAL),
    RMSA = ifelse(is.null(model$rms), NA_real_, model$rms),
    RMSA_corrected = ifelse(is.null(model$crms), NA_real_, model$crms),
    TLI = ifelse(is.null(model$TLI), NA_real_, model$TLI),
    RMSEA = ifelse(is.null(model$RMSEA), NA_real_, model$RMSEA[1]),
    RMSEA_CI = ifelse(is.null(model$RMSEA), NA_real_, 0.9),
    RMSEA_CI_low = ifelse(is.null(model$RMSEA), NA_real_, model$RMSEA[2]),
    RMSEA_CI_high = ifelse(is.null(model$RMSEA), NA_real_, model$RMSEA[3]),
    BIC = ifelse(is.null(model$BIC), NA_real_, model$BIC)
  )

  if (all(metrics == "all")) {
    metrics <- names(out)
  }

  # clean up
  out <- out[, metrics]
  out <- datawizard::remove_empty_columns(out)

  class(out) <- c("performance_fa", "performance_model", class(out))
  out
}

#' @export
model_performance.principal <- model_performance.fa

#' @export
model_performance.parameters_efa <- function(model, metrics = "all", verbose = TRUE, ...) {
  model_performance(attributes(model)$model, metrics = metrics, verbose = verbose, ...)
}

#' @export
model_performance.omega <- function(model, metrics = "all", verbose = TRUE, ...) {
  # extract model object from item_omega
  if (inherits(model, "item_omega")) {
    n_factors <- attr(model, "n", exact = TRUE)
    model <- attributes(model)$model
  } else {
    # number of factors?
    n_factors <- ifelse(is.null(model$Call$nfactors), 3, model$Call$nfactors)
  }

  # generate statistics for n-factor solution and g-model
  out <- do.call(rbind, lapply(list(model$schmid, model$gstats), function(stats) {
    data.frame(
      Chi2 = ifelse(is.null(stats$STATISTIC), NA_real_, stats$STATISTIC),
      df = ifelse(is.null(stats$dof), NA_real_, stats$dof),
      p_Chi2 = ifelse(is.null(stats$PVAL), NA_real_, stats$PVAL),
      RMSA = ifelse(is.null(stats$rms), NA_real_, stats$rms),
      RMSA_corrected = ifelse(is.null(stats$crms), NA_real_, stats$crms),
      TLI = ifelse(is.null(stats$TLI), NA_real_, stats$TLI),
      RMSEA = ifelse(is.null(stats$RMSEA), NA_real_, stats$RMSEA[1]),
      RMSEA_CI = ifelse(is.null(stats$RMSEA), NA_real_, 0.9),
      RMSEA_CI_low = ifelse(is.null(stats$RMSEA), NA_real_, stats$RMSEA[2]),
      RMSEA_CI_high = ifelse(is.null(stats$RMSEA), NA_real_, stats$RMSEA[3]),
      BIC = ifelse(is.null(stats$BIC), NA_real_, stats$BIC),
      R2 = ifelse(is.null(stats$R2), NA_real_, stats$R2),
      Correlation = ifelse(is.null(stats$R2), NA_real_, sqrt(stats$R2))
    )
  }))

  # bind first column, to indicate component
  out <- cbind(
    data.frame(Model = c(sprintf("%i-factor solution", n_factors), "g-model")),
    out
  )

  if (all(metrics == "all")) {
    metrics <- names(out)
  }

  # clean up
  out <- out[, metrics]
  out <- datawizard::remove_empty_columns(out)

  attr(out, "n") <- n_factors
  attr(out, "model") <- model
  class(out) <- unique(c("performance_omega", "performance_fa", "performance_model", class(out)))
  out
}

#' @export
model_performance.item_omega <- model_performance.omega


# methods ----------------------------------

#' @export
print.performance_omega <- function(x, ...) {
  print.performance_model(x, ...)
  n <- attr(x, "n", exact = TRUE)
  insight::print_color(
    insight::format_message(sprintf(
      "\nCompare the model fit of the %i-factor solution with the g-only model. If the g-model is better your items likely describe a single unidimensional construct. If the %i-factor model has better fit then your construct is likely made up of %i sub-constructs.",
      n, n, n
    )),
    "yellow"
  )
}
