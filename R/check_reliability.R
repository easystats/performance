#' Variability-Over-Uncertainty Ratio (D-vour) for Random Effects Reliability
#'
#' @description TODO: Add description.
#'
#' @param x A model object.
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
  check_reliability(modelbased::estimate_grouplevel(x, ...), ...)
}


#' @export
check_reliability.estimate_grouplevel <- function(x, ...) {
  coefname <- attributes(x)$coef_name
  dispname <- names(x)[grep("SE|SD|MAD", names(x))]

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
        rez$Reliability2 <- mean(d[[coefname]]^2 / (d[[coefname]]^2 + d[[dispname]]^2))

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
