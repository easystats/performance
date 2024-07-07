#' @title Ferrari's and Cribari-Neto's R2
#' @name r2_ferrari
#'
#' @description Calculates Ferrari's and Cribari-Neto's pseudo R2 (for
#' beta-regression models).
#'
#' @param model Generalized linear, in particular beta-regression model.
#' @param ... Currently not used.
#'
#' @return A list with the pseudo R2 value.
#'
#' @references
#' - Ferrari, S., and Cribari-Neto, F. (2004). Beta Regression for Modelling Rates
#'   and Proportions. Journal of Applied Statistics, 31(7), 799–815.
#'   \doi{10.1080/0266476042000214501}
#'
#' @examplesIf require("betareg")
#' data("GasolineYield", package = "betareg")
#' model <- betareg::betareg(yield ~ batch + temp, data = GasolineYield)
#' r2_ferrari(model)
#' @export
r2_ferrari <- function(model, ...) {
  UseMethod("r2_ferrari")
}

#' @export
r2_ferrari.default <- function(model, ...) {
  # on the reponse scale, beta regression link doesn't workd
  predictions <- stats::predict(model, type = "response")
  eta <- insight::link_function(model)(predictions)
  y <- insight::get_response(model)

  ferrari <- stats::cor(eta, insight::link_function(model)(y))^2
  out <- list(R2 = c(`Ferrari's R2` = ferrari))

  attr(out, "model_type") <- "Generalized Linear"
  structure(class = "r2_generic", out)
}


# helper -----------------------------

# .r2_ferrari <- function(model, x) {
#   if (inherits(model, "glmmTMB")) {
#     insight::check_if_installed("lme4")
#     # coefficients, but remove phi parameter
#     x <- .collapse_cond(lme4::fixef(model))
#     x <- x[names(x) != "(phi)"]
#   } else {
#     # coefficients, but remove phi parameter
#     x <- stats::coef(model)
#     x <- x[names(x) != "(phi)"]
#   }

#   # model matrix, check dimensions / length
#   mm <- insight::get_modelmatrix(model)

#   if (length(x) != ncol(mm)) {
#     insight::format_warning("Model matrix and coefficients do not match.")
#     return(NULL)
#   }

#   # linear predictor for the mean
#   eta <- as.vector(x %*% t(mm))
#   y <- insight::get_response(model)

#   ferrari <- stats::cor(eta, insight::link_function(model)(y))^2
#   out <- list(R2 = c(`Ferrari's R2` = ferrari))

#   attr(out, "model_type") <- "Generalized Linear"
#   structure(class = "r2_generic", out)
# }
