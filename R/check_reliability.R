#' Variability-Over-Uncertainty Ratio (D-vour) for Random Effects Reliability
#'
#' @description TODO: Add description.
#'
#' @param x A model object (or from [`modelbased::estimate_grouplevel()`]).
#' @param n_trials to do...
#' @param ... Currently not used.
#'
#
#'
#' @details TODO: Add details.
#'
#' @references TODO.
#'
#' @family functions to check model assumptions and and assess model quality
#'
#' @examplesIf require("lme4")
#' # Add groups to the data
#' data <- iris
#' data$Place <- as.factor(rep(c("P1", "P2", "P3", "P4", "P5", "P6"), each = 25))
#'
#' # lme4
#' m <- lme4::lmer(Sepal.Width ~ Petal.Width + (Petal.Width | Place), data = data)
#' check_reliability(m)
#'
#' @export
check_reliability <- function(x, ...) {
  UseMethod("check_reliability")
}


#' @export
check_reliability.default <- function(x, ...) {
  insight::check_if_installed("modelbased", minimum_version = "0.10.0")
  check_reliability(modelbased::estimate_grouplevel(x, ...), ...)
}


#' @rdname check_reliability
#' @export
check_reliability.estimate_grouplevel <- function(x, n_trials = NULL, ...) {
  coefname <- attributes(x)$coef_name
  dispname <- grep("SE|SD|MAD", colnames(x), value = TRUE)

  # extract model information, to get number of trials
  model <- attributes(x)$model
  model_data <- insight::get_data(model)

  # if number of trials not yet specified, define it by number of values from
  # random slopes. this is assumed to be the number of trials
  if (is.null(n_trials)) {
    # find random slopes
    random_slopes <- unique(unlist(
      insight::find_random_slopes(model),
      use.names = FALSE
    ))
    # if we have any, get data and count unique values for number of trials,
    # and extract random effects variances to calculate gamma-squared
    if (!is.null(random_slopes)) {
      n_trials <- .safe(insight::n_unique(unlist(
        lapply(model_data[random_slopes], unique),
        use.names = FALSE
      )))
      gamma2 <- .safe(.extract_reliability_gamma(model))
    }
  }

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

        # Raw Signal-to-Noise Ratio
        rez <- data.frame(
          Component = comp,
          Group = grp,
          Parameter = param,
          Variability = stats::sd(d[[coefname]]),
          Uncertainty = mean(d[[dispname]])
        )
        rez$Reliability <- rez$Variability / rez$Uncertainty

        # Alternative: average of level-specific reliability
        # suggested by @DominiqueMakowski, to have this index bound to 0-1
        rez$Reliability2 <- mean(d[[coefname]]^2 / (d[[coefname]]^2 + d[[dispname]]^2))

        # Alternative 2: like hlmer?
        # Suggested by @strengejacke, to have this index bound to 0-1, but using
        # SD^2 (i.e. Variability^2) is actually the "tau" value from the Rouders
        # paper https://journals.sagepub.com/doi/10.1177/09637214231220923
        rez$Reliability3 <- rez$Variability^2 / (rez$Variability^2 + rez$Uncertainty^2)

        # Alternative 3: the index from the paper?
        if (!is.null(n_trials) && !is.null(gamma2)) {
          rez$Reliability4 <- .expected_reliability(n_trials, gamma2)
        }

        # TODO: we probably need to pick one reliability index

        reliability <- rbind(reliability, rez)
      }
    }
  }

  # Clean-up output
  if (insight::n_unique(reliability$Component) == 1 && unique(reliability$Component) == "TEMP") {
    reliability$Component <- NULL
  }

  reliability
}


# helper functions -----------------------------------

.extract_reliability_gamma <- function(model) {
  v <- insight::get_variance(model)
  var_between <- v$var.intercept
  var_within <- v$var.residual
  var_between / var_within
}

# see https://journals.sagepub.com/doi/10.1177/09637214231220923
.expected_reliability <- function(n_trials, gamma) {
  # it's actually gamma-squared,
  gamma / (gamma + (2 / n_trials))
}
