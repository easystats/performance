#' @title Compute Likelihood-Ratio-Test for Model Comparison
#' @name performance_lrt
#'
#' @description Compute Likelihood-Ratio-Test for Model Comparison
#'
#' @param ... Multiple model objects, which should respond to \code{anova()}.
#'
#' @return Numeric, the AICc value.
#'
#' @details (This only make statistical sense if the models are nested.) It is conventional to list the models from smallest to largest, but this is up to the user.
#'
#' @examples
#' m1 <- lm(mpg ~ wt + cyl, data = mtcars)
#' m2 <- lm(mpg ~ wt + cyl + gear, data = mtcars)
#' m3 <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
#' performance_lrt(m1, m2, m3)
#'
#' @export
performance_lrt <- function(...) {
  UseMethod("performance_lrt")
}


#' @importFrom insight is_model
#' @export
performance_lrt.default <- function(...) {
  if (!all(sapply(list(...), insight::is_model))) {
    stop("All objects must be valid regression model objects!")
  }

  # LRT for model comparison
  if (length(list(...)) > 1) {
    # sd_mle <- function(model) sqrt(mean(residuals(model)^2))
    # ll <- function(model, sd) {
    #   sum(dnorm(insight::get_response(model), mean = fitted(model), sd = sd, log = TRUE))
    # }
    # -2 * (ll(m2, sd_mle(m2)) - ll(m3, sd_mle(m3)))
    lrt <- stats::anova(..., test = "LRT")
  }
}
