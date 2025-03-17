#' Random Effects Reliability
#'
#' Variability-Over-Uncertainty Ratio (d-vour).
#'
#' @description TODO: Add description.
#'
#' @param x A model object (or from [`modelbased::estimate_grouplevel()`]).
#' @param ... Currently not used.
#'
#
#'
#' @details TODO: Add details.
#'
#' Interpretation: d-vour corresponds to: between-groups variability /
#' (between-groups variability + within-group variability) A d-vour of 3/4
#' (0.75) means that there is 3 times more variability between groups than
#' within groups (3/1), and a d-vour of less than 0.5 means that there is more
#' variability within groups than between groups (which is bad if the goal is to
#' analyze group-level effects).
#'
#' @references TODO.
#'
#'
#' @examplesIf require("lme4") && require("glmmTMB")
#' df <- read.csv("https://raw.githubusercontent.com/easystats/circus/refs/heads/main/data/illusiongame.csv")
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
#' m <- lme4::lmer(RT ~ Illusion_Difference + (Illusion_Difference | Participant) + (1 | Trial), data = df)
#' performance_reliability(m)
#' performance_dvour(m)
#'
#' m <- glmmTMB::glmmTMB(RT ~ Illusion_Difference + (Illusion_Difference | Participant) + (1 | Trial), data = df)
#' performance_reliability(m)
#' performance_dvour(m)
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

  original_params <- parameters::parameters(
    x,
    effects = "random",
    group_level = TRUE
  )
  params <- as.data.frame(original_params)

  # bayesian model? if yes, copy clean parameter names
  if (isTRUE(insight::model_info(x)$is_bayesian)) {
    params$Parameter <- attributes(original_params)$pretty_labels
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
      # "What part of reliability is invariant to trial size? Consider the ratio sigma_B^2 / sigma_W^2.
      # This is a signal-to-noise variance ratio - it is how much more variable people are relative to
      # trial noise. Let gamma2 denote this ratio. With it, the reliability coefficient follows (eq. 1):
      # E(r) = gamma2 / (gamma2 + 2/L)" (or 1/L for non-contrast tasks, see annotation 4)

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

      # The parameter γ is the signal-to-noise standard-deviation ratio. It is often convenient for
      # communication as standard deviations are sometimes more convenient than variances.
      # rez$Reliability_adjusted <- sqrt(rez$Reliability_adjusted)

      reliability <- rbind(reliability, rez)
    }
  }

  reliability
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
