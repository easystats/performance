#' @title Kullback-Leibler R2
#' @name r2_kl
#'
#' @description Calculates the Kullback-Leibler-divergence-based
#'   R2 for generalized linear models.
#'
#' @param model A generalized linear model.
#' @param adjust Logical, if \code{TRUE} (the default), the adjusted R2 value is returned.
#' @param ... Currently not used.
#'
#' @examples
#' model <- glm(vs ~ wt + mpg, data = mtcars, family = "binomial")
#' r2_kl(model)
#'
#' @references Cameron, A. C. and Windmeijer, A. G. (1997) An R-squared measure of goodness of fit for some common nonlinear regression models. Journal of Econometrics, 77: 329-342.
#'
#' @export
r2_kl <- function(model, ...) {
  UseMethod("r2_kl")
}

#' @export
r2_kl.glm <- function(model, adjust = TRUE, ...) {
  if (adjust)
    adj <- model$df.null/model$df.residual
  else
    adj <- 1
  1 - (model$deviance / model$null.deviance) * adj
}
