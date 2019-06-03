#' @export
print.check_model <- function(x, ...) {
  if (!requireNamespace("see", quietly = TRUE)) {
    stop("Package 'see' required to plot model assumptions. Please install it.")
  }
  NextMethod()
}



#' @importFrom insight print_color
#' @export
print.check_distribution <- function(x, ...) {
  insight::print_color("# Distribution of Model Family\n\n", "blue")

  x1 <- x[order(x$p_Residuals, decreasing = TRUE)[1:3], c(1, 2)]
  x1 <- x1[x1$p_Residuals > 0, ]
  x1$p_Residuals <- sprintf("%g%%", 100 * x1$p_Residuals)
  colnames(x1) <- c("Distribution", "Probability")

  insight::print_color("Predicted Distribution of Residuals\n\n", "red")
  print.data.frame(x1, row.names = FALSE, ...)

  x2 <- x[order(x$p_Response, decreasing = TRUE)[1:3], c(1, 3)]
  x2 <- x2[x2$p_Response > 0, ]
  x2$p_Response <- sprintf("%g%%", 100 * x2$p_Response)
  colnames(x2) <- c("Distribution", "Probability")

  insight::print_color("\nPredicted Distribution of Response\n\n", "red")
  print.data.frame(x2, row.names = FALSE, ...)
}



#' @importFrom bayestestR area_under_curve
#' @importFrom insight print_color
#' @export
print.performance_roc <- function(x, ...) {
  if (length(unique(x$Model)) == 1) {
    cat(sprintf("AUC: %.2f%%\n", 100 * bayestestR::area_under_curve(x$Sensivity, x$Specifity)))
  } else {
    insight::print_color("# Area under Curve\n\n", "blue")

    dat <- split(x, f = x$Model)
    max_space <- max(nchar(x$Model))

    for (i in 1:length(dat)) {
      cat(sprintf(
        "  %*s: %.2f%%\n",
        max_space,
        names(dat)[i],
        100 * bayestestR::area_under_curve(dat[[i]]$Sensivity, dat[[i]]$Specifity)
      ))
    }
  }
}


#' @importFrom insight print_color
#' @export
print.item_difficulty <- function(x, ...) {
  spaces <- max(nchar(x$item))

  insight::print_color("# Item Difficulty\n\n", "blue")
  insight::print_color(sprintf("  %*s  ideal\n", spaces + 10, "difficulty"), "red")

  for (i in 1:length(x$item)) {
    cat(sprintf("  %*s      %.2f   %.2f\n",  spaces, x$item[i], x$difficulty[i], x$ideal[i]))
  }
}


#' @importFrom insight print_color
#' @export
print.performance_pcp <- function(x, digits = 2, ...) {
  insight::print_color("# Percentage of Correct Predictions from Logistic Regression Model\n\n", "blue")
  cat(sprintf("  Full model: %.2f%% [%.2f%% - %.2f%%]\n", 100 * x$pcp_model, 100 * x$model_ci_low, 100 * x$model_ci_high))
  cat(sprintf("  Null model: %.2f%% [%.2f%% - %.2f%%]\n", 100 * x$pcp_m0, 100 * x$null_ci_low, 100 * x$null_ci_high))

  insight::print_color("\n# Likelihood-Ratio-Test\n\n", "blue")

  v1 <- sprintf("%.3f", x$lrt_chisq)
  v2 <- sprintf("%.3f", x$lrt_p)

  space <- max(nchar(c(v1, v2)))

  cat(sprintf("  Chi-squared: %*s\n", space, v1))
  cat(sprintf("      p-value: %*s\n\n", space, v2))
}



#' @importFrom insight print_color
#' @export
print.looic <- function(x, digits = 2, ...) {
  insight::print_color("# LOOIC and ELPD with Standard Error\n\n", "blue")

  out <- paste0(c(
    sprintf("  LOOIC: %.*f [%.*f]", digits, x$LOOIC, digits, x$LOOIC_SE),
    sprintf("   ELPD: %.*f [%.*f]", digits, x$ELPD, digits, x$ELPD_SE)),
    collapse = "\n"
  )

  cat(out)
  cat("\n")
}



#' @importFrom insight print_color
#' @export
print.r2_generic <- function(x, digits = 3, ...) {
  model_type <- attr(x, "model_type")
  if (!is.null(model_type)) {
    insight::print_color(sprintf("# R2 for %s Regression\n\n", model_type), "blue")
  } else {
    insight::print_color("# R2\n\n", "blue")
  }

  out <- paste0(c(
    sprintf("       R2: %.*f", digits, x$R2),
    sprintf("  adj. R2: %.*f", digits, x$R2_adjusted)),
    collapse = "\n"
  )

  cat(out)
  cat("\n")
}



#' @importFrom insight print_color
#' @export
print.r2_nakagawa <- function(x, digits = 3, ...) {
  insight::print_color("# R2 for mixed models\n\n", "blue")

  out <- paste0(c(
    sprintf("  Conditional R2: %.*f", digits, x$R2_conditional),
    sprintf("     Marginal R2: %.*f", digits, x$R2_marginal)),
    collapse = "\n"
  )

  cat(out)
  cat("\n")
}



#' @importFrom insight print_color
#' @export
print.r2_bayes <- function(x, digits = 3, ...) {
  insight::print_color("# Bayesian R2 with Standard Error\n\n", "blue")

  out <- sprintf("  Conditional R2: %.*f [%.*f]", digits, x$R2_Bayes, digits, attr(x, "std.error")[["R2_Bayes"]])

  if ("R2_Bayes_marginal" %in% names(x)) {
    out <- paste0(c(out,  sprintf("     Marginal R2: %.*f [%.*f]", digits, x$R2_Bayes_marginal, digits, attr(x, "std.error")[["R2_Bayes_marginal"]])), collapse = "\n")
  }

  cat(out)
  cat("\n")
}



#' @export
print.perf_pca_rotate <- function(x, cutoff = 0.1, digits = 3, ...) {

  insight::print_color(sprintf("# Rotated loadings from Principal Component Analysis (%s-rotation)\n\n", attr(x, "rotation", exact = TRUE)), "blue")

  xs <- attr(x, "variance", exact = TRUE)
  x <- round(x, digits = digits)

  x <- as.data.frame(apply(x, MARGIN = c(1, 2), function(.y) {
    if (abs(.y) < cutoff)
      ""
    else
      as.character(.y)
  }), stringsAsFactors = FALSE)

  xs <- as.data.frame(t(as.data.frame(round(xs, digits = digits))))

  colnames(xs) <- sprintf("PC%i", 1:ncol(xs))
  rownames(xs) <- c("Proportion variance", "Cumulative variance", "Proportion explained", "Cumulative explained")

  print(x, quote = FALSE, ...)
  insight::print_color("\n(Explained) Variance\n", "cyan")
  print(xs, ...)
}



#' @export
print.perf_pca <- function(x, digits = 3, ...) {
  x <- as.data.frame(round(x, digits = digits))
  rownames(x) <- c("Standard deviation", "Eigenvalue", "Proportion variance", "Cumulative variance")
  print(x, ...)
}



#' @importFrom insight print_color
#' @export
print.icc <- function(x, digits = 3, ...) {
  insight::print_color("# Intraclass Correlation Coefficient\n\n", "blue")

  out <- paste0(c(
    sprintf("     Adjusted ICC: %.*f", digits, x$ICC_adjusted),
    sprintf("  Conditional ICC: %.*f", digits, x$ICC_conditional)),
    collapse = "\n"
  )

  cat(out)
  cat("\n")
}



#' @importFrom insight print_color
#' @export
print.check_zi <- function(x, ...) {
  insight::print_color("# Check for zero-inflation\n\n", "blue")
  cat(sprintf("   Observed zeros: %i\n", x$observed.zeros))
  cat(sprintf("  Predicted zeros: %i\n", x$predicted.zeros))
  cat(sprintf("            Ratio: %.2f\n\n", x$ratio))

  lower <- 1 - x$tolerance
  upper <- 1 + x$tolerance

  if (x$ratio < lower)
    message("Model is underfitting zeros (probable zero-inflation).")
  else if (x$ratio > upper)
    message("Model is overfitting zeros.")
  else
    message("Model seems ok, ratio of observed and predicted zeros is within the tolerance range.")
}



#' @export
print.check_overdisp <- function(x, digits = 3, ...) {
  x$dispersion_ratio <- sprintf("%.*f", digits, x$dispersion_ratio)
  x$chisq_statistic <- sprintf("%.*f", digits, x$chisq_statistic)

  x$p_value <- pval <- round(x$p_value, digits = digits)
  if (x$p_value < .001) x$p_value <- "< 0.001"

  maxlen <- max(
    nchar(x$dispersion_ratio),
    nchar(x$chisq_statistic),
    nchar(x$p_value)
  )

  insight::print_color("# Overdispersion test\n\n", "blue")
  cat(sprintf("       dispersion ratio = %s\n", format(x$dispersion_ratio, justify = "right", width = maxlen)))
  cat(sprintf("  Pearson's Chi-Squared = %s\n", format(x$chisq_statistic, justify = "right", width = maxlen)))
  cat(sprintf("                p-value = %s\n\n", format(x$p_value, justify = "right", width = maxlen)))

  if (pval > 0.05)
    message("No overdispersion detected.")
  else
    message("Overdispersion detected.")
}



#' @importFrom insight print_color
#' @export
print.icc_decomposed <- function(x, digits = 2, ...) {
  # print model information
  cat("# Random Effect Variances and ICC\n\n")

  reform <- attr(x, "re.form", exact = TRUE)
  if (is.null(reform))
    reform <- "all random effects"
  else
    reform <- deparse(reform)

  cat(sprintf("Conditioned on: %s\n\n", reform))

  prob <- attr(x, "ci", exact = TRUE)

  cat(insight::print_color("## Variance Ratio (comparable to ICC)\n", "blue"))

  icc.val <- sprintf("%.*f", digits, x$ICC_decomposed)

  ci.icc.lo <- sprintf("%.*f", digits, x$ICC_CI[1])
  ci.icc.hi <- sprintf("%.*f", digits, x$ICC_CI[2])

  # ICC
  cat(sprintf(
    "Ratio: %s  CI %i%%: [%s %s]\n",
    icc.val,
    as.integer(round(prob * 100)),
    ci.icc.lo,
    ci.icc.hi
  ))

  cat(insight::print_color("\n## Variances of Posterior Predicted Distribution\n", "blue"))

  null.model <- sprintf("%.*f", digits, attr(x, "var_rand_intercept", exact = TRUE))

  ci.null <- attr(x, "ci.var_rand_intercept", exact = TRUE)
  ci.null.lo <- sprintf("%.*f", digits, ci.null$CI_low)
  ci.null.hi <- sprintf("%.*f", digits, ci.null$CI_high)

  full.model <- sprintf("%.*f", digits, attr(x, "var_total", exact = TRUE))

  ci.full <- attr(x, "ci.var_total", exact = TRUE)
  ci.full.lo <- sprintf("%.*f", digits, ci.full$CI_low)
  ci.full.hi <- sprintf("%.*f", digits, ci.full$CI_high)

  ml <- max(nchar(null.model), nchar(full.model))
  ml.ci <- max(nchar(ci.full.lo), nchar(ci.null.lo))
  mh.ci <- max(nchar(ci.full.hi), nchar(ci.null.hi))

  # Conditioned on fixed effects
  cat(sprintf(
    "Conditioned on fixed effects: %*s  CI %i%%: [%*s %*s]\n",
    ml,
    null.model,
    as.integer(round(prob * 100)),
    ml.ci,
    ci.null.lo,
    mh.ci,
    ci.null.hi
  ))

  # Conditioned on random effects
  cat(sprintf(
    "Conditioned on rand. effects: %*s  CI %i%%: [%*s %*s]\n",
    ml,
    full.model,
    as.integer(round(prob * 100)),
    ml.ci,
    ci.full.lo,
    mh.ci,
    ci.full.hi
  ))

  cat(insight::print_color("\n## Difference in Variances\n", "red"))

  res <- sprintf("%.*f", digits, attr(x, "var_residual", exact = TRUE))

  ci.res <- attr(x, "ci.var_residual", exact = TRUE)
  ci.res.lo <- sprintf("%.*f", digits, ci.res$CI_low)
  ci.res.hi <- sprintf("%.*f", digits, ci.res$CI_high)

  # ICC
  cat(sprintf(
    "Difference: %s  CI %i%%: [%s %s]\n",
    res,
    as.integer(round(prob * 100)),
    ci.res.lo,
    ci.res.hi
  ))
}



#' @export
print.binned_residuals <- function(x, ...) {
  if (!requireNamespace("see", quietly = TRUE)) {
    stop("Package 'see' required to plot binned residuals. Please install it.")
  }
  NextMethod()
}



#' @export
print.performance_hosmer <- function(x, ...) {
  insight::print_color("# Hosmer-Lemeshow Goodness-of-Fit Test\n\n", "blue")

  v1 <- sprintf("%.3f", x$chisq)
  v2 <- sprintf("%i    ", x$df)
  v3 <- sprintf("%.3f", x$p.value)

  space <- max(nchar(c(v1, v2, v3)))

  cat(sprintf("  Chi-squared: %*s\n", space, v1))
  cat(sprintf("           df: %*s\n", space, v2))
  cat(sprintf("      p-value: %*s\n\n", space, v3))

  if (x$p.value >= 0.05)
    message("Summary: model seems to fit well.")
  else
    message("Summary: model does not fit well.")
}


#' @export
print.performance_accuracy <- function(x, ...) {
  # headline
  insight::print_color("# Accuracy of Model Predictions\n\n", "blue")

  # statistics
  cat(sprintf("Accuracy: %.2f%%\n", 100 * x$accuracy))
  cat(sprintf("      SE: %.2f%%-points\n", 100 * x$std.error))
  cat(sprintf("  Method: %s\n", x$stat))
}


#' @export
print.performance_score <- function(x, ...) {
  # headline
  insight::print_color("# Proper Scoring Rules\n\n", "blue")

  results <- format(
    c(
      sprintf("%.4f", x$logarithmic),
      sprintf("%.4f", x$quadratic),
      sprintf("%.4f", x$spherical)
    ),
    justify = "right")

  cat(sprintf("logarithmic: %s\n", results[1]))
  cat(sprintf("  quadratic: %s\n", results[2]))
  cat(sprintf("  spherical: %s\n", results[3]))
}


#' @export
print.check_collinearity <- function(x, ...) {
  insight::print_color("# Check for Multicollinearity\n", "blue")

  if ("Component" %in% colnames(x)) {
    comp <- split(x, x$Component)
    for (i in 1:length(comp)) {
      cat(paste0("\n* ", comp[[i]]$Component[1], " component:\n"))
      .print_collinearity(comp[[i]][, 1:3])
    }
  } else {
    .print_collinearity(x)
  }
}


.print_collinearity <- function(x) {
  vifs <- x$VIF

  x$VIF <- sprintf("%.2f", x$VIF)
  x$SE_factor <- sprintf("%.2f", x$SE_factor)

  colnames(x)[3] <- "Increased SE"

  low_corr <- which(vifs < 5)
  if (length(low_corr)) {
    cat("\n")
    insight::print_color("Low Correlation\n\n", "green")
    print.data.frame(x[low_corr, ], row.names = FALSE)
  }

  mid_corr <- which(vifs >= 5 & vifs < 10)
  if (length(mid_corr)) {
    cat("\n")
    insight::print_color("Moderate Correlation\n\n", "yellow")
    print.data.frame(x[mid_corr, ], row.names = FALSE)
  }

  high_corr <- which(vifs >= 10)
  if (length(high_corr)) {
    cat("\n")
    insight::print_color("High Correlation\n\n", "red")
    print.data.frame(x[high_corr, ], row.names = FALSE)
  }
}