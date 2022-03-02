#' @title Check model for violation of sphericity
#' @name check_sphericity
#'
#' @description Check model for violation of sphericity
#'
#' @param x A model object.
#' @param ... Arguments passed to `car::Anova`.
#'
#' @return Invisibly returns the p-values of the test statistics. A p-value <
#'   0.05 indicates a violation of sphericity.
#'
#' @examples
#' if (require("car")) {
#'   soils.mod <- lm(
#'     cbind(pH, N, Dens, P, Ca, Mg, K, Na, Conduc) ~ Block + Contour * Depth,
#'     data = Soils
#'   )
#'
#'   check_sphericity(Manova(soils.mod))
#' }
#' @export
check_sphericity <- function(x, ...) {
  UseMethod("check_sphericity")
}



# default --------------------------

#' @export
check_sphericity.default <- function(x, ...) {
  stop("Test not supported yet for object of class ", class(x)[1])
}



# methods ------------------------------

#' @export
plot.check_sphericity <- function(x, ...) {
  warning(insight::format_message("There is currently no plot() method for `check_sphericity()`."), call. = FALSE)
}


#' @export
print.check_sphericity <- function(x, ...) {
  if (any(x < .05)) {
    pp <- x[x < .05]
    pp <- paste0("\n - ", names(pp), " (", insight::format_p(pp), ")", collapse = "")
    insight::print_color(sprintf("Warning: Sphericity violated for: %s.\n", pp), "red")
  } else {
    pp <- insight::format_p(min(x))
    pp <- sub("=", ">", pp)
    insight::print_color(sprintf("OK: Data seems to be spherical (%s).\n", pp), "green")
  }
  invisible(x)
}



# other classes ------------------

#' @export
check_sphericity.Anova.mlm <- function(x, ...) {
  S <- summary(x, multivariate = FALSE, univariate = TRUE)
  test <- S$sphericity.tests

  p.val <- test[, 2]
  class(p.val) <- c("check_sphericity", "see_check_sphericity", class(p.val))
  p.val
}


#' @export
check_sphericity.afex_aov <- function(x, ...) {
  if (length(attr(x, "within")) == 0) {
    stop("Mauchly Test of Sphericity is only aplicable to ANOVAs with within-subjects factors.")
  }

  check_sphericity.Anova.mlm(x, ...)
}


#' @export
check_sphericity.mlm <- function(x, ...) {
  if (!requireNamespace("car")) {
    stop("car required for this function to work.")
  }

  check_sphericity.Anova.mlm(car::Anova(x, ...))
}
