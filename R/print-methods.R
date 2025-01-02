#' @export
print.r2_generic <- function(x, digits = 3, ...) {
  model_type <- attr(x, "model_type")
  if (!is.null(model_type)) {
    insight::print_color(sprintf("# R2 for %s Regression\n", model_type), "blue")
  }

  if (all(c("R2_adjusted", "R2_within_adjusted") %in% names(x))) {
    # print regular R2
    out <- c(
      sprintf("              R2: %.*f", digits, x$R2[1]),
      sprintf("         adj. R2: %.*f", digits, x$R2_adjusted[1]),
      sprintf("       within R2: %.*f", digits, x$R2_within[1]),
      sprintf("  adj. within R2: %.*f", digits, x$R2_within_adjusted[1])
    )
  } else if ("R2_adjusted" %in% names(x)) {
    out <- c(
      sprintf("       R2: %.*f", digits, x$R2[1]),
      sprintf("  adj. R2: %.*f", digits, x$R2_adjusted[1])
    )
  } else {
    out <- sprintf("  %s: %.*f", names(x$R2[1]), digits, x$R2[1])
  }

  # add CI?
  if (length(x$R2) == 3) {
    out[1] <- .add_r2_ci_to_print(out[1], x$R2[2], x$R2[3], digits = digits)
  }
  if (!is.null(x$R2_adjusted) && length(x$R2_adjusted) == 3 && length(out) > 1) {
    out[2] <- .add_r2_ci_to_print(out[2], x$R2_adjusted[2], x$R2_adjusted[3], digits = digits)
  }
  if (!is.null(x$R2_within) && length(x$R2_within) == 3 && length(out) > 2) {
    out[3] <- .add_r2_ci_to_print(out[3], x$R2_within[2], x$R2_within[3], digits = digits)
  }
  if (!is.null(x$R2_within_adjusted) && length(x$R2_within_adjusted) == 3 && length(out) > 3) {
    out[4] <- .add_r2_ci_to_print(out[4], x$R2_within_adjusted[2], x$R2_within_adjusted[3], digits = digits)
  }

  # separate lines for multiple R2
  out <- paste0(out, collapse = "\n")

  cat(out)
  cat("\n")
  invisible(x)
}

.add_r2_ci_to_print <- function(out, ci_low, ci_high, digits) {
  paste0(
    out,
    sprintf(" %s", insight::format_ci(ci_low, ci_high, digits = digits, ci = NULL))
  )
}


#' @export
print.r2_pseudo <- function(x, digits = 3, ...) {
  model_type <- attr(x, "model_type")
  if (!is.null(model_type)) {
    insight::print_color(sprintf("# R2 for %s Regression\n", model_type), "blue")
  }
  cat(sprintf("  %s: %.*f\n", names(x[[1]]), digits, x[[1]]))
  invisible(x)
}


#' @export
print.r2_mlm <- function(x, digits = 3, ...) {
  model_type <- attr(x, "model_type")
  is_multivar_r2 <- all(names(x) == c("Symmetric Rxy", "Asymmetric Pxy"))
  if (!is.null(model_type) && !is_multivar_r2) {
    insight::print_color(
      sprintf("# R2 for %s Regression\n\n", model_type),
      "blue"
    )
  } else if (!is.null(model_type) && is_multivar_r2) {
    insight::print_color(
      sprintf("# Multivariate R2 for %s Regression\n", model_type),
      "blue"
    )
  } else {
    insight::print_color("# R2\n\n", "blue")
  }

  if (is_multivar_r2) {
    cat(sprintf(" Symmetric Rxy: %.*f", digits, x[["Symmetric Rxy"]]))
    cat("\n")
    cat(sprintf("Asymmetric Pxy: %.*f", digits, x[["Asymmetric Pxy"]]))
    cat("\n\n")
  } else {
    for (i in names(x)) {
      insight::print_color(sprintf("## %s\n", i), "cyan")
      out <- paste(
        c(
          sprintf("        R2: %.*f", digits, x[[i]]$R2),
          sprintf("   adj. R2: %.*f", digits, x[[i]]$R2_adjusted)
        ),
        collapse = "\n"
      )
      cat(out)
      cat("\n\n")
    }
  }
  invisible(x)
}


#' @export
print.r2_bayes <- function(x, digits = 3, ...) {
  insight::print_color("# Bayesian R2 with Compatibility Interval\n\n", "blue")

  r2_ci <- insight::format_ci(
    attributes(x)$CI$R2_Bayes$CI_low,
    attributes(x)$CI$R2_Bayes$CI_high,
    ci = attributes(x)$CI$R2_Bayes$CI,
    digits = digits
  )
  out <- sprintf("  Conditional R2: %.*f (%s)", digits, x$R2_Bayes, r2_ci)

  if ("R2_Bayes_marginal" %in% names(x)) {
    r2_marginal_ci <- insight::format_ci(
      attributes(x)$CI$R2_Bayes_marginal$CI_low,
      attributes(x)$CI$R2_Bayes_marginal$CI_high,
      ci = attributes(x)$CI$R2_Bayes_marginal$CI,
      digits = digits
    )
    out <- paste(c(
      out,
      sprintf("     Marginal R2: %.*f (%s)", digits, x$R2_Bayes_marginal, r2_marginal_ci)
    ), collapse = "\n")
  }

  cat(out)
  cat("\n")
  invisible(x)
}


#' @export
print.r2_loo <- function(x, digits = 3, ...) {
  insight::print_color("# LOO-adjusted R2 with Compatibility Interval\n\n", "blue")

  r2_ci <- insight::format_ci(
    attributes(x)$CI$R2_loo$CI_low,
    attributes(x)$CI$R2_loo$CI_high,
    ci = attributes(x)$CI$R2_loo$CI,
    digits = digits
  )
  out <- sprintf("  Conditional R2: %.*f (%s)", digits, x$R2_loo, r2_ci)

  if ("R2_loo_marginal" %in% names(x)) {
    r2_marginal_ci <- insight::format_ci(
      attributes(x)$CI$R2_loo_marginal$CI_low,
      attributes(x)$CI$R2_loo_marginal$CI_high,
      ci = attributes(x)$CI$R2_loo_marginal$CI,
      digits = digits
    )
    out <- paste(c(
      out,
      sprintf("     Marginal R2: %.*f (%s)", digits, x$R2_loo_marginal, r2_marginal_ci)
    ), collapse = "\n")
  }

  cat(out)
  cat("\n")
  invisible(x)
}


#' @export
print.r2_nakagawa_by_group <- function(x, digits = 3, ...) {
  insight::print_color("# Explained Variance by Level\n\n", "blue")
  cat(insight::export_table(x, digits = digits))
  cat("\n")
  invisible(x)
}
