#' @title Interval Odds Ratio
#' @name performance_ior
#'
#' @description
#' The Interval Odds Ratio (IOR) evaluates the fixed effect of a cluster-level
#' (level 2) covariate by explicitly incorporating the residual between-cluster
#' heterogeneity.
#'
#' @param x A (logistic) multilevel model.
#'
#' @seealso [`performance_poor()`] and [`performance_mor()`] as additional
#' metrics specifically for logistic multilevel regression models, and [`icc()`]
#' for multilevel models in general.
#'
#' @details
#' Unlike a standard confidence interval (which reflects sample estimation
#' uncertainty around the coefficients), the IOR reflects the variation in odds
#' ratios across clusters due to residual cluster heterogeneity.
#'
#' - *IOR does not contain 1:* If the entire interval is above 1 (or below 1),
#'   the cluster-level covariate has a strong effect. Even when moving from a
#'   "good" unexposed cluster to a "bad" exposed cluster (or vice versa), the
#'   directional effect of the covariate remains dominant.
#' - *IOR contains 1:* When the interval contains 1, the between-cluster
#'   heterogeneity is larger than the effect of the covariate itself. This means
#'   an individual moving from an unexposed cluster to an exposed cluster could
#'   actually experience lower odds of the outcome if the new cluster happens to
#'   have a very low unobserved random effect.
#'
#' @return
#' A data frame with the parameter names and their interval odds ratios.
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
#' performance_ior(m)
#'
#' m <- suppressWarnings(lme4::glmer(
#'   high_reaction ~ Days + (1 | mygrp) + (1 | Subject),
#'   data = sleepstudy,
#'   family = "binomial"
#' ))
#' performance_ior(m)
#' @export
performance_ior <- function(x) {
  model_info <- insight::model_info(x)

  valid_ior <- .valid_roc_models(x) &&
    any(unlist(
      model_info[c("is_binomial", "is_ordinal", "is_multinomial", "is_cumulative")],
      use.names = FALSE
    )) &&
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
        CI = 0.8,
        CI_low = exp(params$Estimate + stats::qnorm(0.1) * sqrt(2 * v_a[tau])),
        CI_high = exp(params$Estimate + stats::qnorm(0.9) * sqrt(2 * v_a[tau])),
        stringsAsFactors = FALSE
      )
    })
  )

  class(out) <- c("performance_ior", "data.frame")
  out
}

#' @export
print.performance_ior <- function(x, ...) {
  cat(insight::export_table(insight::format_table(x), caption = "Interval Odds Ratio"))
  invisible(x)
}
