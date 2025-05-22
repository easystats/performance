#' Random Effects Reliability
#'
#' @description These functions provide information about the reliability of
#' group-level estimates (i.e., random effects) in mixed models. They are useful
#' to assess whether the predictors yield consistent group-level variability.
#' "Group-level" can refer, for instance, to different participants in a study,
#' and the predictors to the effect of some experimental condition.
#'
#' The conceptually related functions are implemented,
#' `performance_reliability()`, based on Rouder & Mehrvarz (2024) that uses
#' estimated model variances, and `performance_dvour()` (d-vour), which
#' corresponds to the Variability-Over-Uncertainty Ratio ("vour") between random
#' effects coefficient variability and their associated uncertainty.
#'
#' **Note**: `performance_reliability()` requires to recompute the model to
#' estimate some of the variances of interest, which does not make it very
#' usable with Bayesian models. Please get in touch if you have would like to
#' help addressing this.
#'
#' @param x A model object.
#' @param ... Currently not used.
#'
#' @details
#'
#' ## Reliability (Signal-to-Noise Ratio)
#'
#' `performance_reliability()` estimates the reliability of random effects
#' (intercepts and slopes) in mixed-effects models using variance decomposition.
#' This method follows the **hierarchical modeling** framework of Rouder &
#' Mehrvarz (2024), defining reliability as the **signal-to-noise variance
#' ratio**:
#'
#' \deqn{\gamma^2 = \frac{\sigma_B^2}{\sigma_B^2 + \sigma_W^2}}
#'
#' where:
#' - \eqn{\sigma_B^2} is the **between-subject variance** (i.e., variability
#'   across groups).
#' - \eqn{\sigma_W^2} is the **within-subject variance** (i.e., trial-level
#'   measurement noise).
#'
#' This metric quantifies **how much observed variability is due to actual
#' differences between groups**, rather than measurement error or within-group
#' fluctuations.
#'
#' To account for **trial count (\eqn{L})**, reliability is adjusted following:
#'
#' \deqn{E(r) = \frac{\gamma^2}{\gamma^2 + 1/L}}
#'
#' where \eqn{L} is the number of **observations per random effect level** (note
#' that Rouder (2024) recommends 2/L to adjust for contrast effects).
#'
#' ## Variability-Over-Uncertainty Ratio (d-vour)
#'
#' `performance_dvour()` computes an alternative reliability measure corresponding
#' to the normalized **ratio of observed variability to uncertainty in random effect
#' estimates**. This is defined as:
#'
#' \deqn{\text{D-vour} = \frac{\sigma_B^2}{\sigma_B^2 + \mu_{\text{SE}}^2}}
#'
#' where:
#' - \eqn{\sigma_B^2} is the **between-group variability** (computed as the SD
#'   of the random effect estimates).
#' - \eqn{\mu_{\text{SE}}^2} is the **mean squared uncertainty** in random
#'   effect estimates (i.e., the average uncertainty).
#'
#' ### Interpretation:
#'
#' - **D-vour > 0.75**: Strong group-level effects (between-group variance is at
#'   least 3 times greater than uncertainty).
#' - **D-vour ~ 0.5**: Within-group and between-group variability are similar;
#'   random effect estimates should be used with caution.
#' - **D-vour < 0.5**: Measurement noise dominates; random effect estimates are
#'   probably unreliable.
#'
#' While d-vour shares some similarity to Rouder's Reliability, it does not
#' explicitly model within-group trial-level noise and is only based on the
#' random effect estimates, and can thus be not accurate when there is not
#' a lot of random factor groups (the reliability of this index -
#' the meta-reliability - depends on the number of groups).
#'
#' @references
#' - Rouder, J. N., Pena, A. L., Mehrvarz, M., & Vandekerckhove, J. (2024). On
#'   Cronbach’s merger: Why experiments may not be suitable for measuring
#'   individual differences.
#' - Rouder, J. N., & Mehrvarz, M. (2024). Hierarchical-model insights for
#'   planning and interpreting individual-difference studies of cognitive
#'   abilities. Current Directions in Psychological Science, 33(2), 128-135.
#' - Williams, D. R., Mulder, J., Rouder, J. N., & Rast, P. (2021). Beneath the
#'   surface: Unearthing within-person variability and mean relations with
#'   Bayesian mixed models. Psychological methods, 26(1), 74.
#' - Williams, D. R., Martin, S. R., DeBolt, M., Oakes, L., & Rast, P. (2020). A
#'   fine-tooth comb for measurement reliability: Predicting true score and
#'   error variance in hierarchical models.
#'
#' @examplesIf all(insight::check_if_installed(c("lme4", "glmmTMB"), quietly = TRUE))
#' url <- "https://raw.githubusercontent.com/easystats/circus/refs/heads/main/data/illusiongame.csv"
#' df <- read.csv(url)
#'
#' m <- lme4::lmer(RT ~ (1 | Participant), data = df)
#' performance_reliability(m)
#' performance_dvour(m)
#'
#' m <- glmmTMB::glmmTMB(RT ~ (1 | Participant), data = df)
#' performance_reliability(m)
#' performance_dvour(m)
#'
#' m <- lme4::lmer(RT ~ (1 | Participant) + (1 | Trial), data = df)
#' performance_reliability(m)
#' performance_dvour(m)
#'
#' m <- glmmTMB::glmmTMB(RT ~ (1 | Participant) + (1 | Trial), data = df)
#' performance_reliability(m)
#' performance_dvour(m)
#'
#' \donttest{
#' m <- lme4::lmer(
#'   RT ~ Illusion_Difference + (Illusion_Difference | Participant) + (1 | Trial),
#'   data = df
#' )
#' performance_reliability(m)
#' performance_dvour(m)
#'
#' m <- glmmTMB::glmmTMB(
#'   RT ~ Illusion_Difference + (Illusion_Difference | Participant) + (1 | Trial),
#'   data = df
#' )
#' performance_reliability(m)
#' performance_dvour(m)
#' }
#' @export
performance_reliability <- function(x, ...) {
  UseMethod("performance_reliability")
}


#' @export
performance_reliability.default <- function(x, ...) {
  # sanity check
  if (!insight::is_model(x)) {
    insight::format_error("`x` must be a regression model object.")
  }

  # Find how many observations per random effect (n-trials)
  random <- lapply(insight::get_random(x), function(z) min(table(z)))
  v <- insight::get_variance(x) # Extract variance components

  original_params <- parameters::parameters(x, effects = "grouplevel", verbose = FALSE)
  params <- as.data.frame(original_params)

  # bayesian model? if yes, copy clean parameter names
  if (isTRUE(insight::model_info(x)$is_bayesian)) {
    if (inherits(x, "brmsfit")) {
      params$Parameter <- gsub("(.*)\\[(.*),(.*)\\]", "\\3", params$Parameter)
    } else if (inherits(x, c("stanreg", "stanfit"))) {
      params$Parameter <- gsub("b\\[(.*)\\s(.*)", "\\1", params$Parameter)
    }
  }

  reliability <- data.frame()
  for (grp in unique(params$Group)) {
    for (param in unique(params$Parameter)) {
      # Store group-level results
      rez <- data.frame(
        Group = grp,
        Parameter = param
      )

      # Based on Rouder's (2024) paper https://journals.sagepub.com/doi/10.1177/09637214231220923
      # "What part of reliability is invariant to trial size? Consider the ratio
      # sigma_B^2 / sigma_W^2. This is a signal-to-noise variance ratio - it is
      # how much more variable people are relative to trial noise. Let gamma2
      # denote this ratio. With it, the reliability coefficient follows (eq. 1):
      # E(r) = gamma2 / (gamma2 + 2/L)" (or 1/L for non-contrast tasks, see
      # annotation 4)

      # Number of trials per group
      L <- random[[grp]]

      # Extract variances
      if (param %in% c("(Intercept)", "Intercept")) {
        var_between <- v$var.intercept[grp]
      } else {
        var_between <- v$var.slope[paste0(grp, ".", param)]
      }

      # Non-adjusted index
      # rez$Reliability <- var_between / (var_between + v$var.residual)

      # Adjusted index:
      # Rouder & Mehrvarz suggest 1/L for non-contrast tasks and 2/L for contrast tasks.
      rez$Reliability <- var_between / (var_between + v$var.residual + 1 / L)

      # The parameter γ is the signal-to-noise standard-deviation ratio. It is
      # often convenient for communication as standard deviations are sometimes
      # more convenient than variances.
      # rez$Reliability_adjusted <- sqrt(rez$Reliability_adjusted)

      reliability <- rbind(reliability, rez)
    }
  }

  # clean
  reliability[!is.na(reliability$Reliability), ]
}


# d-vour ------------------------------------------------------------------


#' @rdname performance_reliability
#' @export
performance_dvour <- function(x, ...) {
  UseMethod("performance_dvour")
}


#' @export
performance_dvour.default <- function(x, ...) {
  insight::check_if_installed("modelbased", minimum_version = "0.10.0")
  performance_dvour(modelbased::estimate_grouplevel(x, ...), ...)
}


#' @export
performance_dvour.estimate_grouplevel <- function(x, ...) {
  coefname <- attributes(x)$coef_name
  dispname <- grep("SE|SD|MAD", colnames(x), value = TRUE)

  # Sanity checks
  if (insight::n_unique(x$Level) <= 3) {
    insight::format_alert(paste0(
      "The number of random effects group levels (N=",
      insight::n_unique(x$Level),
      ") might be too low to reliably estimate the variability."
    ))
  }

  if (length(dispname) == 0) {
    insight::format_error(paste0(
      "This function requires an index of variability of each random ",
      "effect (e.g., SE) but none was found. Try running `check_reliability()` on the ",
      "output of `modelbased::estimate_grouplevel(model)`, and make sure the latter ",
      "returns a table with an index of dispersion."
    ))
  }

  if (length(dispname) > 1) {
    insight::format_alert(paste0(
      "Multiple indices of variability were found (",
      toString(dispname),
      "). Using the first one."
    ))
    dispname <- dispname[1]
  }

  # Compute reliability
  if (!"Component" %in% names(x)) x$Component <- "TEMP"

  reliability <- data.frame()

  for (comp in unique(x$Component)) {
    for (grp in unique(x$Group)) {
      for (param in unique(x$Parameter)) {
        d <- x[x$Component == comp & x$Group == grp & x$Parameter == param, ]
        if (nrow(d) == 0) next

        # Store group-level results
        rez <- data.frame(
          Component = comp,
          Group = grp,
          Parameter = param
        )

        # Variability-Over-Uncertainty Ratio (d-vour)
        # This index is based on the information contained in the group-level estimates.
        var_between <- stats::sd(d[[coefname]]) # Variability
        var_within <- mean(d[[dispname]]) # Average Uncertainty

        rez$D_vour <- var_between^2 / (var_between^2 + var_within^2)

        # Alternative 1: average of level-specific reliability
        # Inspired by the hlmer package (R version of HLM7 by Raudenbush et al., 2014)
        # rez$Dvour2 <- mean(d[[coefname]]^2 / (d[[coefname]]^2 + d[[dispname]]^2))

        reliability <- rbind(reliability, rez)
      }
    }
  }

  # Clean-up output
  if (insight::has_single_value(reliability$Component, remove_na = TRUE) && unique(reliability$Component) == "TEMP") {
    reliability$Component <- NULL
  }

  reliability
}
