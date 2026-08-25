#' @title Median Odds Ratio
#' @name performance_mor
#'
#' @description
#' A measure of cluster-level variation in multilevel logistic regression,
#' defined as the median odds ratio between two randomly chosen individuals from
#' different clusters with identical covariates, comparing the person at higher
#' risk to the person at lower risk.
#'
#' @param x A (logistic) multilevel model.
#'
#' @seealso [`performance_ior()`] and [`performance_poor()`] as additional
#' metrics specifically for logistic multilevel regression models, and [`icc()`]
#' for multilevel models in general.
#'
#' @details
#' The MOR is always greater than or equal to 1 and can be interpreted as follows:
#' - *MOR close to 1:* No Cluster Effect. There is (almost) no between-cluster
#'   heterogeneity, meaning cluster membership plays no role in the outcome.
#' - *MOR > 1:* Presence of Heterogeneity. Indicates meaningful variation across
#'   clusters. If two persons are randomly picked from two different clusters,
#'   the MOR represents the median factor by which the odds of the outcome
#'   increase for the individual from the higher-risk cluster compared to the
#'   individual in the lower-risk cluster.
#'
#' @return
#' A data frame with two columns, one with the group (cluster) names and one
#' with the median odds ratios.
#'
#' @references
#' Larsen K, Merlo J. Appropriate Assessment of Neighborhood Effects on Individual
#' Health: Integrating Random and Fixed Effects in Multilevel Logistic
#' Regression. American Journal of Epidemiology (2005) 161:81–88.
#' \doi{10.1093/aje/kwi017}
#'
#' Merlo J, Wagner P, Ghith N, Leckie G. An Original Stepwise Multilevel
#' Logistic Regression Analysis of Discriminatory Accuracy: The Case of
#' Neighbourhoods and Health. PLoS ONE (2016) 11:e0153778.
#' \doi{10.1371/journal.pone.0153778}
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
#' performance_mor(m)
#'
#' m <- suppressWarnings(lme4::glmer(
#'   high_reaction ~ Days + (1 | mygrp) + (1 | Subject),
#'   data = sleepstudy,
#'   family = "binomial"
#' ))
#' performance_mor(m)
#'
#' @export
performance_mor <- function(x) {
  model_info <- insight::model_info(x)

  valid_mor <- .valid_roc_models(x) &&
    isTRUE(model_info$is_logit) &&
    isTRUE(model_info$is_binomial) &&
    insight::is_mixed_model(x)

  if (!valid_mor) {
    insight::format_error("The supplied model needs to be a logistic multilevel model.")
  }

  v_a <- insight::get_variance_intercept(x)
  mor <- exp(sqrt(2 * v_a) * stats::qnorm(0.75))

  out <- data.frame(
    Group = gsub("var.intercept.", "", names(v_a), fixed = TRUE),
    MOR = mor,
    stringsAsFactors = FALSE
  )

  class(out) <- c("performance_mor", "data.frame")
  out
}

#' @export
print.performance_mor <- function(x, ...) {
  cat(insight::export_table(x, caption = "Median Odds Ratio"))
  invisible(x)
}
