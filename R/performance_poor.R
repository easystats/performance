#' @title Proportion of Opposed Odds Ratios
#' @name performance_poor
#'
#' @description
#' This function quantifies the extent to which the residual (unexplained)
#' stratum heterogeneity dominates the estimated fixed effect of a group- or
#' stratum-level characteristic. This measure represents the percentage of all
#' hypothetical, pairwise comparisons in which the individual-level effect is in
#' the opposite direction of the estimated average odds ratio (OR).
#'
#' @param x A (logistic) multilevel model.
#'
#' @details
#' The POOR value always ranges between 0% and 50%.
#' - 0%: Perfect homogeneity. Every single randomly selected pair of
#'   observations shows an effect in the same direction as the overall OR.
#' - 50%: Total heterogeneity (pure chance). In exactly half of the cases, the
#'   effect is reversed; the stratum characteristic possesses no orderly
#'   predictive power whatsoever, as it is completely dominated by the
#'   unexplained stratum variance.
#'
#' Practical Significance: A high POOR value (e.g., > 30%) serves as a strong
#' warning to researchers against making overgeneralized statements about the
#' utility of a stratum characteristic, as the average OR masks massive
#' internal heterogeneity.
#'
#' @seealso [`performance_ior()`] and [`performance_mor()`] as additional
#' metrics specifically for logistic multilevel regression models, and [`icc()`]
#' for multilevel models in general.
#'
#' @return
#' A data frame with the parameter names and their POOR estimates.
#'
#' @examplesIf all(insight::check_if_installed(c("lme4", "datawizard"), quietly = TRUE))
#' data(sleepstudy, package = "lme4")
#' sleepstudy$mygrp <- sample(1:5, size = 180, replace = TRUE)
#' sleepstudy$high_reaction <- as.factor(datawizard::categorize(sleepstudy$Reaction))
#'
#' m <- lme4::glmer(
#'   high_reaction ~ Days + (1 | Subject),
#'   data = sleepstudy,
#'   family = "binomial"
#' )
#' performance_poor(m)
#'
#' m <- suppressWarnings(lme4::glmer(
#'   high_reaction ~ Days + (1 | mygrp) + (1 | Subject),
#'   data = sleepstudy,
#'   family = "binomial"
#' ))
#' performance_poor(m)
#' @export
performance_poor <- function(x) {
  model_info <- insight::model_info(x)

  valid_ior <- .valid_roc_models(x) &&
    isTRUE(model_info$is_logit) &&
    isTRUE(model_info$is_binomial) &&
    insight::is_mixed_model(x)

  if (!valid_ior) {
    insight::format_error("The supplied model needs to be a logistic multilevel model.")
  }

  v_a <- insight::get_variance_intercept(x)
  params <- insight::get_parameters(x, effects = "fixed")

  out <- do.call(
    rbind,
    lapply(names(v_a), function(tau) {
      data.frame(
        Parameter = params$Parameter,
        Group = gsub("var.intercept.", "", tau, fixed = TRUE),
        POOR = stats::pnorm(-abs(params$Estimate) / sqrt(2 * v_a[tau])),
        stringsAsFactors = FALSE
      )
    })
  )

  class(out) <- c("performance_poor", "data.frame")
  out
}

#' @export
print.performance_poor <- function(x, ...) {
  cat(insight::export_table(x, caption = "Proportion of Opposed Odds Ratios"))
  invisible(x)
}
