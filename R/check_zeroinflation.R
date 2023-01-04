#' @title Check for zero-inflation in count models
#' @name check_zeroinflation
#'
#' @description `check_zeroinflation()` checks whether count models are
#'   over- or underfitting zeros in the outcome.
#'
#' @param x Fitted model of class `merMod`, `glmmTMB`, `glm`,
#'    or `glm.nb` (package \pkg{MASS}).
#' @param tolerance The tolerance for the ratio of observed and predicted
#'    zeros to considered as over- or underfitting zeros. A ratio
#'    between 1 +/- `tolerance` is considered as OK, while a ratio
#'    beyond or below this threshold would indicate over- or underfitting.
#'
#' @return A list with information about the amount of predicted and observed
#'    zeros in the outcome, as well as the ratio between these two values.
#'
#' @details If the amount of observed zeros is larger than the amount of
#'   predicted zeros, the model is underfitting zeros, which indicates a
#'   zero-inflation in the data. In such cases, it is recommended to use
#'   negative binomial or zero-inflated models.
#'
#' @examples
#' if (require("glmmTMB")) {
#'   data(Salamanders)
#'   m <- glm(count ~ spp + mined, family = poisson, data = Salamanders)
#'   check_zeroinflation(m)
#' }
#' @export
check_zeroinflation <- function(x, tolerance = 0.05) {
  # check if we have poisson
  model_info <- insight::model_info(x)
  if (!model_info$is_count) {
    insight::format_error("Model must be from Poisson-family.")
  }

  # get actual zero of response
  obs.zero <- sum(insight::get_response(x, verbose = FALSE) == 0L)

  if (obs.zero == 0) {
    insight::print_color("Model has no observed zeros in the response variable.\n", "red")
    return(NULL)
  }

  # get predictions of outcome
  mu <- stats::fitted(x)

  # get overdispersion parameters
  if (model_info$is_negbin) {
    if (methods::is(x, "glmmTMB")) {
      theta <- stats::sigma(x)
    } else if (methods::is(x, "glmerMod")) {
      theta <- environment(x@resp$family$aic)[[".Theta"]]
    } else {
      theta <- x$theta
    }
  } else {
    theta <- NULL
  }

  # get predicted zero-counts
  if (!is.null(theta)) {
    pred.zero <- round(sum(stats::dnbinom(x = 0, size = theta, mu = mu)))
  } else {
    pred.zero <- round(sum(stats::dpois(x = 0, lambda = mu)))
  }

  # proportion
  structure(
    class = "check_zi",
    list(
      predicted.zeros = pred.zero,
      observed.zeros = obs.zero,
      ratio = pred.zero / obs.zero,
      tolerance = tolerance
    )
  )
}



# methods ------------------

#' @export
print.check_zi <- function(x, ...) {
  insight::print_color("# Check for zero-inflation\n\n", "blue")
  cat(sprintf("   Observed zeros: %i\n", x$observed.zeros))
  cat(sprintf("  Predicted zeros: %i\n", x$predicted.zeros))
  cat(sprintf("            Ratio: %.2f\n\n", x$ratio))

  lower <- 1 - x$tolerance
  upper <- 1 + x$tolerance

  if (x$ratio < lower) {
    message("Model is underfitting zeros (probable zero-inflation).")
  } else if (x$ratio > upper) {
    message("Model is overfitting zeros.")
  } else {
    message(insight::format_message("Model seems ok, ratio of observed and predicted zeros is within the tolerance range."))
  }

  invisible(x)
}
