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
  if(length(unique(x$Level)) <= 3) {
    warning(paste0("The number of random levels (N = ",
                   length(unique(x$Level)),
                   ") might be too low to reliably estimate the variability."))
  }

  if(length(dispname) == 0) {
    stop(paste0("This function requires an index of variability of each random ",
    "effect (e.g., SE) but none was found. Try running check_reliability() on the",
    " output of modelbased::estimate_grouplevel(model), and make sure the latter ",
    "returns a table with an index of dispersion."))
  }

  if(length(dispname) > 1) {
    warning(paste0("Multiple indices of variability were found (",
                   paste(dispname, collapse = ", "),
                   "). Using the first one."))
    dispname <- dispname[1]
  }


  # Compute reliability
  if (!"Component" %in% names(x)) x$Component <- "TEMP"

  reliability <- data.frame()
  for(c in unique(x$Component)) {
    for(g in unique(x$Group)) {
      for(p in unique(x$Parameter)) {
        d <- x[x$Component == c & x$Group == g & x$Parameter == p,]
        rez <- data.frame(
          Component = c,
          Group = g,
          Parameter = p,
          Variability = sd(d[[coefname]]),
          Uncertainty = mean(d[[dispname]])
        )
        rez$Reliability <- rez$Variability / rez$Uncertainty

        reliability <- rbind(reliability, rez)
      }
    }
  }

  # Clean-up output
  if(length(unique(reliability$Component)) == 1 && unique(reliability$Component) == "TEMP") {
    reliability$Component <- NULL
  }

  reliability
}
