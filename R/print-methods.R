#' @export
print.r2_generic <- function(x, digits = 3, ...) {
  model_type <- attr(x, "model_type")
  if (!is.null(model_type)) {
    insight::print_color(sprintf("# R2 for %s Regression\n", model_type), "blue")
  }

  if (all(c("R2_adjusted", "R2_within_adjusted") %in% names(x))) {
    out <- c(
      sprintf("              R2: %.*f", digits, x$R2),
      sprintf("         adj. R2: %.*f", digits, x$R2_adjusted),
      sprintf("       within R2: %.*f", digits, x$R2_within),
      sprintf("  adj. within R2: %.*f", digits, x$R2_within_adjusted)
    )
    if (!is.null(x$CI_low)) {
      out[1] <- paste0(
        out1,
        sprintf(" %s", insight::format_ci(x$CI_low, x$CI_high, digits = digits, ci = NULL))
      )
    }
    out <- paste0(out, collapse = "\n")
  } else if ("R2_adjusted" %in% names(x)) {
    out <- paste0(
      c(
        sprintf("       R2: %.*f", digits, x$R2),
        sprintf("  adj. R2: %.*f", digits, x$R2_adjusted)
      ),
      collapse = "\n"
    )
  } else {
    out <- sprintf("  %s: %.*f", names(x$R2), digits, x$R2)
  }

  cat(out)
  cat("\n")
  invisible(x)
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
  if (!is.null(model_type)) {
    insight::print_color(sprintf("# R2 for %s Regression\n\n", model_type), "blue")
  } else {
    insight::print_color("# R2\n\n", "blue")
  }

  for (i in names(x)) {
    insight::print_color(sprintf("## %s\n", i), "cyan")
    out <- paste0(
      c(
        sprintf("        R2: %.*f", digits, x[[i]]$R2),
        sprintf("   adj. R2: %.*f", digits, x[[i]]$R2_adjusted)
      ),
      collapse = "\n"
    )
    cat(out)
    cat("\n\n")
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
    out <- paste0(c(out, sprintf("     Marginal R2: %.*f (%s)", digits, x$R2_Bayes_marginal, r2_marginal_ci)), collapse = "\n")
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
    out <- paste0(c(out, sprintf("     Marginal R2: %.*f (%s)", digits, x$R2_loo_marginal, r2_marginal_ci)), collapse = "\n")
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
